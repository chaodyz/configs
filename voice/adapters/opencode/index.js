import { spawn } from "node:child_process";
import path from "node:path";
import { fileURLToPath } from "node:url";

const here = path.dirname(fileURLToPath(import.meta.url));
const defaultVoice = path.resolve(here, "../../bin/voice");

function runProcess(command, args, input = null) {
  return new Promise((resolve, reject) => {
    const child = spawn(command, args, { stdio: ["pipe", "pipe", "pipe"] });
    let stdout = "";
    let stderr = "";
    child.stdout.on("data", (chunk) => (stdout += chunk));
    child.stderr.on("data", (chunk) => (stderr += chunk));
    child.on("error", reject);
    child.on("close", (code) => {
      if (code === 0) resolve(stdout.trim());
      else reject(new Error(stderr.trim() || `${command} exited ${code}`));
    });
    if (input) child.stdin.write(input);
    child.stdin.end();
  });
}

function runVoice(args, input = null) {
  return runProcess(process.env.VOICE_CLI || defaultVoice, args, input);
}

async function appendPrompt(client, text, submit) {
  let result = await client.tui.appendPrompt({ body: { text } });
  if (result?.error?.data?.message === "Expected object, got undefined") {
    result = await client.tui.appendPrompt({ text });
  }
  if (result?.error) throw new Error(result.error.data?.message || "appendPrompt failed");
  if (submit) await client.tui.submitPrompt();
}

async function lastAssistantText(api) {
  const route = api.route.current;
  if (route.name !== "session") return null;
  const messages = api.state.session.messages(route.params.sessionID) || [];
  const assistant = [...messages].reverse().find((message) => message.role === "assistant");
  if (!assistant) return null;
  const response = await api.client.session.message(
    { sessionID: route.params.sessionID, messageID: assistant.id },
    { throwOnError: true },
  );
  return (response.data?.parts || [])
    .filter((part) => part.type === "text")
    .map((part) => part.text || "")
    .join("\n\n")
    .trim();
}

async function assistantTextByID(api, sessionID, messageID) {
  const response = await api.client.session.message(
    { sessionID, messageID },
    { throwOnError: true },
  );
  return (response.data?.parts || [])
    .filter((part) => part.type === "text")
    .map((part) => part.text || "")
    .join("\n\n")
    .trim();
}

export function isFinalAssistantMessage(info) {
  return (
    info?.role === "assistant" &&
    Boolean(info.time?.completed) &&
    !info.error &&
    info.finish !== "tool-calls"
  );
}

async function selectedText(api) {
  const selection = api.renderer.getSelection()?.getSelectedText()?.trim();
  if (selection) return selection;
  if (process.platform !== "darwin") return null;
  const clipboard = await runProcess("/usr/bin/pbpaste", []);
  return clipboard.trim() || null;
}

export class MarkdownSentenceBuffer {
  constructor() {
    this.pending = "";
    this.prose = "";
    this.inCodeFence = false;
  }

  reset() {
    this.pending = "";
    this.prose = "";
    this.inCodeFence = false;
  }

  appendProse(text) {
    this.prose += text.replace(/\s+/g, " ");
  }

  scanPending(final = false) {
    while (this.pending) {
      const fence = this.pending.indexOf("```");
      if (fence !== -1) {
        if (!this.inCodeFence) this.appendProse(this.pending.slice(0, fence));
        this.pending = this.pending.slice(fence + 3);
        this.inCodeFence = !this.inCodeFence;
        continue;
      }

      const trailingTicks = final ? 0 : this.pending.match(/`{1,2}$/)?.[0].length || 0;
      const consumable = this.pending.slice(0, this.pending.length - trailingTicks);
      if (!this.inCodeFence) this.appendProse(consumable);
      this.pending = this.pending.slice(this.pending.length - trailingTicks);
      break;
    }
  }

  takeSentences() {
    const sentences = [];
    while (true) {
      const match = this.prose.match(/^\s*(.+?[.!?])(?=\s|$)/s);
      if (!match) break;
      sentences.push(match[1].trim());
      this.prose = this.prose.slice(match[0].length);
    }
    return sentences;
  }

  push(delta) {
    this.pending += delta;
    this.scanPending(false);
    return this.takeSentences();
  }

  flush() {
    this.scanPending(true);
    const sentences = this.takeSentences();
    const remainder = this.prose.trim();
    if (remainder) sentences.push(remainder);
    this.reset();
    return sentences;
  }
}

export class StreamingSpeaker {
  constructor(onError, run = runVoice) {
    this.buffer = new MarkdownSentenceBuffer();
    this.queue = [];
    this.speaking = false;
    this.generation = 0;
    this.stopping = Promise.resolve();
    this.onError = onError;
    this.run = run;
  }

  push(delta) {
    for (const text of this.buffer.push(delta)) this.queue.push({ text, summary: true });
    this.pump();
  }

  flush() {
    for (const text of this.buffer.flush()) this.queue.push({ text, summary: true });
    this.pump();
  }

  async interrupt() {
    this.generation += 1;
    this.queue = [];
    this.buffer.reset();
    this.stopping = this.run(["stop-speaking"]).catch(() => {});
    await this.stopping;
  }

  async speakNow(text, summary = false) {
    await this.interrupt();
    this.queue.push({ text, summary });
    this.pump();
  }

  async pump() {
    if (this.speaking) return;
    this.speaking = true;
    const generation = this.generation;
    try {
      await this.stopping;
      while (this.queue.length && generation === this.generation) {
        const item = this.queue.shift();
        const args = ["speak"];
        if (item.summary) args.push("--summary");
        await this.run(args, item.text);
      }
    } catch (error) {
      if (generation === this.generation) this.onError(error);
    } finally {
      this.speaking = false;
      if (this.queue.length) this.pump();
    }
  }
}

export default {
  id: "shared-voice",
  tui: async (api) => {
    let recording = false;
    let processing = false;
    let streamSource = null;
    let activeStreamSession = null;
    const spokenMessages = new Set();
    const stats = { status: 0, legacyDelta: 0, nextDelta: 0, acceptedDelta: 0 };
    const toast = (message, variant = "info") =>
      api.ui.toast({ message, variant, duration: 3000 });
    const speaker = new StreamingSpeaker((error) => toast(error.message, "error"));
    const speechMode = () => api.kv.get("voice.tts.mode", "final");
    const autoSpeech = () => speechMode() !== "off";
    const liveSpeech = () => speechMode() === "live";
    const currentSessionID = () =>
      api.route.current.name === "session" ? api.route.current.params.sessionID : null;
    const isActiveSession = (sessionID) => {
      const current = currentSessionID();
      return sessionID === current || sessionID === activeStreamSession;
    };

    async function startRecording() {
      if (processing || recording) return;
      await speaker.interrupt();
      try {
        try {
          await runVoice(["record"]);
        } catch (error) {
          if (!error.message.includes("recording already active")) throw error;
          await runVoice(["cancel"]);
          await runVoice(["record"]);
        }
        recording = true;
        toast("Recording... press Ctrl-x g to transcribe and submit");
      } catch (error) {
        toast(error.message, "error");
      }
    }

    async function finishRecording(submit) {
      if (!recording || processing) return;
      processing = true;
      toast("Transcribing...");
      try {
        const text = await runVoice(["stop"]);
        await appendPrompt(api.client, text, submit);
        toast(submit ? "Transcription submitted" : "Transcription added", "success");
      } catch (error) {
        toast(error.message, "error");
      } finally {
        recording = false;
        processing = false;
      }
    }

    async function cancelRecording() {
      if (!recording && !processing) return;
      try {
        await runVoice(["cancel"]);
      } catch (error) {
        toast(error.message, "error");
      } finally {
        recording = false;
        processing = false;
        toast("Recording cancelled");
      }
    }

    api.event.on("session.status", (event) => {
      stats.status += 1;
      const current = currentSessionID();
      if (event.properties.status.type === "busy") {
        if (current && event.properties.sessionID !== current) return;
        activeStreamSession = event.properties.sessionID;
        streamSource = null;
        speaker.interrupt();
      } else if (
        event.properties.status.type === "idle" &&
        isActiveSession(event.properties.sessionID) &&
        liveSpeech()
      ) {
        speaker.flush();
      }
    });

    api.event.on("message.part.delta", (event) => {
      stats.legacyDelta += 1;
      if (!autoSpeech() || !isActiveSession(event.properties.sessionID)) return;
      if (event.properties.field !== "text") return;
      if (streamSource && streamSource !== "message.part.delta") return;
      streamSource = "message.part.delta";
      stats.acceptedDelta += 1;
      if (liveSpeech()) speaker.push(event.properties.delta);
    });

    api.event.on("session.next.text.delta", (event) => {
      stats.nextDelta += 1;
      if (!autoSpeech() || !isActiveSession(event.properties.sessionID)) return;
      if (streamSource && streamSource !== "session.next.text.delta") return;
      streamSource = "session.next.text.delta";
      stats.acceptedDelta += 1;
      if (liveSpeech()) speaker.push(event.properties.delta);
    });

    api.event.on("message.updated", async (event) => {
      if (speechMode() !== "final" || !isActiveSession(event.properties.sessionID)) return;
      const info = event.properties.info;
      if (!isFinalAssistantMessage(info) || spokenMessages.has(info.id)) return;
      spokenMessages.add(info.id);
      try {
        const text = await assistantTextByID(api, event.properties.sessionID, info.id);
        if (text) await speaker.speakNow(text, true);
      } catch (error) {
        spokenMessages.delete(info.id);
        toast(error.message, "error");
      }
    });

    const commands = [
      {
        title: "Voice: start recording",
        value: "voice.record-start",
        description: "Start local microphone recording",
        keybind: "<leader>r",
        slash: { name: "stt-record" },
        onSelect: startRecording,
      },
      {
        title: "Voice: transcribe and submit",
        value: "voice.record-submit",
        description: "Stop recording, transcribe, and submit the prompt",
        keybind: "<leader>g",
        slash: { name: "stt-submit" },
        onSelect() {
          if (!recording) return toast("No recording is active", "warning");
          return finishRecording(true);
        },
      },
      {
        title: "Voice: cancel recording",
        value: "voice.record-cancel",
        description: "Cancel the current recording",
        keybind: "ctrl+g",
        slash: { name: "stt-stop" },
        onSelect: cancelRecording,
      },
      {
        title: "Voice: speak last response",
        value: "voice.speak-last",
        description: "Interrupt playback and read the full last response",
        keybind: "<leader>a",
        slash: { name: "tts-speak" },
        async onSelect() {
          try {
            const text = await lastAssistantText(api);
            if (!text) return toast("No assistant response to speak", "warning");
            await speaker.speakNow(text, false);
          } catch (error) {
            toast(error.message, "error");
          }
        },
      },
      {
        title: "Voice: speak selected text",
        value: "voice.speak-selection",
        description: "Interrupt playback and read the OpenTUI selection or clipboard",
        keybind: "<leader>shift+s",
        slash: { name: "tts-selection" },
        async onSelect() {
          try {
            const text = await selectedText(api);
            if (!text) return toast("No selected text or clipboard content", "warning");
            await speaker.speakNow(text, false);
          } catch (error) {
            toast(error.message, "error");
          }
        },
      },
      {
        title: "Voice: toggle streaming speech",
        value: "voice.tts-mode",
        description: "Cycle final, live, and off automatic speech modes",
        keybind: "<leader>v",
        slash: { name: "tts-mode" },
        async onSelect() {
          const modes = ["final", "live", "off"];
          const next = modes[(modes.indexOf(speechMode()) + 1) % modes.length];
          api.kv.set("voice.tts.mode", next);
          if (next === "off") await speaker.interrupt();
          toast(`Automatic speech mode: ${next}`);
        },
      },
      {
        title: "Voice: stop speech",
        value: "voice.tts-stop",
        description: "Stop current speech and clear queued sentences",
        keybind: "<leader>s",
        slash: { name: "tts-stop" },
        async onSelect() {
          await speaker.interrupt();
          toast("Speech stopped");
        },
      },
      {
        title: "Voice: status",
        value: "voice.status",
        description: "Show streaming speech diagnostics",
        slash: { name: "voice-status" },
        onSelect() {
          toast(
            `mode=${speechMode()} active=${activeStreamSession || "none"} deltas=${stats.acceptedDelta}/${stats.legacyDelta + stats.nextDelta}`,
          );
        },
      },
    ];

    api.command.register(() => commands);
    api.lifecycle.onDispose(async () => {
      await speaker.interrupt();
      if (recording || processing) {
        try {
          await runVoice(["cancel"]);
        } catch {}
      }
    });
  },
};

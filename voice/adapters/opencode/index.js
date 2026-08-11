import { spawn } from "node:child_process";
import path from "node:path";
import { fileURLToPath } from "node:url";

const here = path.dirname(fileURLToPath(import.meta.url));
const defaultVoice = path.resolve(here, "../../bin/voice");

function runVoice(args, input = null) {
  const command = process.env.VOICE_CLI || defaultVoice;
  return new Promise((resolve, reject) => {
    const child = spawn(command, args, { stdio: ["pipe", "pipe", "pipe"] });
    let stdout = "";
    let stderr = "";
    child.stdout.on("data", (chunk) => (stdout += chunk));
    child.stderr.on("data", (chunk) => (stderr += chunk));
    child.on("error", reject);
    child.on("close", (code) => {
      if (code === 0) resolve(stdout.trim());
      else reject(new Error(stderr.trim() || `voice exited ${code}`));
    });
    if (input) child.stdin.write(input);
    child.stdin.end();
  });
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

export default {
  id: "shared-voice",
  tui: async (api) => {
    let recording = false;
    let processing = false;
    const toast = (message, variant = "info") =>
      api.ui.toast({ message, variant, duration: 3000 });

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

    const commands = [
      {
        title: "Voice: record/transcribe",
        value: "voice.record",
        description: "Toggle local microphone recording and transcription",
        keybind: "ctrl+r",
        slash: { name: "stt-record" },
        async onSelect() {
          if (processing) return toast("Transcription is already running");
          if (recording) return finishRecording(false);
          try {
            await runVoice(["record"]);
            recording = true;
            toast("Recording... press Ctrl-r again to transcribe");
          } catch (error) {
            toast(error.message, "error");
          }
        },
      },
      {
        title: "Voice: transcribe and submit",
        value: "voice.submit",
        description: "Stop recording, transcribe, and submit the prompt",
        keybind: "<leader>r",
        slash: { name: "stt-submit" },
        onSelect() {
          if (!recording) return toast("No recording is active", "warning");
          finishRecording(true);
        },
      },
      {
        title: "Voice: cancel recording",
        value: "voice.cancel",
        description: "Cancel the current recording",
        slash: { name: "stt-stop" },
        async onSelect() {
          try {
            await runVoice(["cancel"]);
            recording = false;
            toast("Recording cancelled");
          } catch (error) {
            toast(error.message, "error");
          }
        },
      },
      {
        title: "Voice: speak last response",
        value: "voice.speak",
        description: "Read a short summary of the last assistant response",
        keybind: "<leader>s",
        slash: { name: "tts-speak" },
        async onSelect() {
          try {
            const text = await lastAssistantText(api);
            if (!text) return toast("No assistant response to speak", "warning");
            await runVoice(["speak", "--summary", "--background"], text);
            toast("Speaking response");
          } catch (error) {
            toast(error.message, "error");
          }
        },
      },
    ];

    api.command.register(() => commands);
  },
};

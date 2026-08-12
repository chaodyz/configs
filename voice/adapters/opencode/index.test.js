import assert from "node:assert/strict";
import test from "node:test";

import voiceAdapter, {
  isFinalAssistantMessage,
  MarkdownSentenceBuffer,
  StreamingSpeaker,
} from "./index.js";

function createAdapterHarness() {
  let commands;
  const api = {
    renderer: {},
    keymap: {},
    mode: { current: () => "base" },
    route: { current: { name: "home" } },
    kv: { get: (_key, fallback) => fallback, set() {} },
    ui: { toast() {} },
    event: { on() {} },
    command: {
      register(callback) {
        commands = callback();
      },
    },
    lifecycle: { onDispose() {} },
    client: {
      tui: {
        appendPrompt: async () => ({}),
        submitPrompt: async () => {},
      },
    },
  };

  return voiceAdapter.tui(api).then(() => ({ commands }));
}

test("streams complete sentences across deltas", () => {
  const buffer = new MarkdownSentenceBuffer();

  assert.deepEqual(buffer.push("First sentence. Sec"), ["First sentence."]);
  assert.deepEqual(buffer.push("ond sentence! Remaining"), ["Second sentence!"]);
  assert.deepEqual(buffer.flush(), ["Remaining"]);
});

test("does not speak fenced code", () => {
  const buffer = new MarkdownSentenceBuffer();

  assert.deepEqual(buffer.push("Changed the function.\n```py\nprint('secret.')"), [
    "Changed the function.",
  ]);
  assert.deepEqual(buffer.push("\n```\nTests pass."), ["Tests pass."]);
  assert.deepEqual(buffer.flush(), []);
});

test("handles code fence markers split across deltas", () => {
  const buffer = new MarkdownSentenceBuffer();

  assert.deepEqual(buffer.push("Before. ``"), ["Before."]);
  assert.deepEqual(buffer.push("`hidden.``` After."), ["After."]);
});

test("waits for interruption before speaking a new streamed sentence", async () => {
  const calls = [];
  let finishStop;
  const stop = new Promise((resolve) => (finishStop = resolve));
  const run = async (args, input) => {
    calls.push([args, input]);
    if (args[0] === "stop-speaking") await stop;
  };
  const speaker = new StreamingSpeaker(() => {}, run);

  const interrupting = speaker.interrupt();
  speaker.push("Streamed sentence.");
  await new Promise((resolve) => setImmediate(resolve));
  assert.deepEqual(calls, [[["stop-speaking"], undefined]]);

  finishStop();
  await interrupting;
  await new Promise((resolve) => setImmediate(resolve));
  assert.deepEqual(calls[1], [["speak", "--summary"], "Streamed sentence."]);
});

test("distinguishes final answers from intermediate tool-call text", () => {
  const base = { role: "assistant", time: { completed: 1 }, id: "message" };

  assert.equal(isFinalAssistantMessage({ ...base, finish: "tool-calls" }), false);
  assert.equal(isFinalAssistantMessage({ ...base, finish: "stop" }), true);
  assert.equal(isFinalAssistantMessage({ ...base, finish: "length" }), true);
  assert.equal(isFinalAssistantMessage({ ...base, finish: "stop", error: {} }), false);
});

test("registers the Ctrl-x voice chord shortcuts", async () => {
  const { commands } = await createAdapterHarness();
  const keybinds = Object.fromEntries(
    commands.map((command) => [command.value, command.keybind]),
  );

  assert.equal(keybinds["voice.tts-stop"], "<leader>s");
  assert.equal(keybinds["voice.record-start"], "<leader>r");
  assert.equal(keybinds["voice.record-submit"], "<leader>g");
  assert.equal(keybinds["voice.speak-last"], "<leader>a");
});

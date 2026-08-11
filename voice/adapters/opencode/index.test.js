import assert from "node:assert/strict";
import test from "node:test";

import voiceAdapter, {
  isFinalAssistantMessage,
  MarkdownSentenceBuffer,
  StreamingSpeaker,
} from "./index.js";

function createAdapterHarness() {
  const editor = {
    inserted: [],
    deleted: 0,
    insertText(text) {
      this.inserted.push(text);
    },
    deleteCharBackward() {
      this.deleted += 1;
    },
  };
  let intercept;
  const api = {
    renderer: {
      currentFocusedEditor: editor,
      enableKittyKeyboard() {},
    },
    keymap: {
      intercept(_kind, callback) {
        intercept = callback;
        return () => {};
      },
    },
    mode: { current: () => "base" },
    route: { current: { name: "home" } },
    kv: { get: (_key, fallback) => fallback, set() {} },
    ui: { toast() {} },
    event: { on() {} },
    command: { register() {} },
    lifecycle: { onDispose() {} },
    client: {
      tui: {
        appendPrompt: async () => ({}),
        submitPrompt: async () => {},
      },
    },
  };

  return voiceAdapter.tui(api).then(() => ({ editor, intercept }));
}

function spaceEvent(eventType = "press") {
  return {
    event: {
      name: "space",
      eventType,
      ctrl: false,
      meta: false,
      shift: false,
      option: false,
    },
    consume() {},
  };
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

test("inserts a tapped Space immediately", async () => {
  const { editor, intercept } = await createAdapterHarness();

  intercept(spaceEvent("press"));
  intercept(spaceEvent("release"));

  assert.deepEqual(editor.inserted, [" "]);
  assert.equal(editor.deleted, 0);
});

test("keeps two separated Space taps instead of treating them as a hold", async () => {
  const { editor, intercept } = await createAdapterHarness();

  intercept(spaceEvent());
  await new Promise((resolve) => setTimeout(resolve, 190));
  intercept(spaceEvent());
  await new Promise((resolve) => setTimeout(resolve, 130));

  assert.deepEqual(editor.inserted, [" ", " "]);
  assert.equal(editor.deleted, 0);
});

test("recognizes tmux-style repeated Space presses as a hold", async () => {
  const originalVoice = process.env.VOICE_CLI;
  process.env.VOICE_CLI = "/usr/bin/true";
  try {
    const { editor, intercept } = await createAdapterHarness();

    intercept(spaceEvent());
    await new Promise((resolve) => setTimeout(resolve, 190));
    intercept(spaceEvent());
    intercept(spaceEvent());

    assert.deepEqual(editor.inserted, [" "]);
    assert.equal(editor.deleted, 1);
    await new Promise((resolve) => setTimeout(resolve, 220));
  } finally {
    if (originalVoice === undefined) delete process.env.VOICE_CLI;
    else process.env.VOICE_CLI = originalVoice;
  }
});

import importlib.util
import json
import os
import subprocess
import sys
import tempfile
import time
import unittest
from pathlib import Path
from unittest.mock import patch


VOICE_PATH = Path(__file__).parents[1] / "voice.py"
SPEC = importlib.util.spec_from_file_location("voice_core", VOICE_PATH)
voice = importlib.util.module_from_spec(SPEC)
assert SPEC.loader
sys.modules[SPEC.name] = voice
SPEC.loader.exec_module(voice)


class VoiceTest(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.root = Path(self.temp.name)
        self.model = self.root / "model.bin"
        self.model.write_bytes(b"model")

    def tearDown(self):
        self.temp.cleanup()

    def config(self, **overrides):
        values = {
            "state_dir": self.root / "state",
            "sox_command": "/usr/bin/false",
            "whisper_command": "/usr/bin/false",
            "whisper_model": self.model,
            "say_command": "/usr/bin/true",
            "microphone": None,
            "voice_name": None,
            "voice_rate": 210,
            "max_summary_chars": 220,
            "claude_wait_seconds": 0,
        }
        values.update(overrides)
        return voice.Config(**values)

    def executable(self, name, body):
        path = self.root / name
        path.write_text(f"#!/usr/bin/env bash\nset -euo pipefail\n{body}\n")
        path.chmod(0o755)
        return str(path)

    def test_summarize_for_speech_removes_code_and_markdown(self):
        text = "# Complete\n**Updated** `voice.py`.\n```python\nsecret = 1\n```\nCheck details."
        self.assertEqual(
            voice.summarize_for_speech(text),
            "Complete. Updated voice.py. Check details.",
        )

    def test_summarize_keeps_going_past_a_long_first_sentence(self):
        # Regression: a hard-coded 120-char soft stop ended every summary after
        # one sentence, because responses routinely open with a longer one.
        first = "The hook fired before the final text block was flushed, so it read the previous turn instead of this one."
        self.assertGreater(len(first), 100)
        summary = voice.summarize_for_speech(f"{first} Second sentence. Third one.")

        self.assertIn("Second sentence.", summary)
        self.assertIn("Third one.", summary)

    def test_summarize_respects_sentence_limit(self):
        text = "One here. Two here. Three here. Four here."
        self.assertEqual(
            voice.summarize_for_speech(text, max_sentences=2),
            "One here. Two here.",
        )

    def test_summarize_soft_stop_is_opt_in(self):
        text = "A first sentence of some length. A second sentence."
        self.assertEqual(
            voice.summarize_for_speech(text, soft_stop=20),
            "A first sentence of some length.",
        )

    def test_summarize_truncates_on_a_word_boundary(self):
        text = "Alpha bravo charlie delta echo foxtrot golf hotel india juliet kilo lima."
        summary = voice.summarize_for_speech(text, max_chars=30)

        self.assertLessEqual(len(summary), 30)
        self.assertTrue(text.startswith(summary))
        # The cut must land between words, never mid-word.
        self.assertEqual(text[len(summary)], " ")

    def test_config_reads_summary_limits_from_env(self):
        with patch.dict(
            os.environ,
            {
                "VOICE_SUMMARY_CHARS": "999",
                "VOICE_SUMMARY_SENTENCES": "7",
                "VOICE_SUMMARY_SOFT_STOP": "150",
            },
        ):
            config = voice.Config.from_env()

        self.assertEqual(config.max_summary_chars, 999)
        self.assertEqual(config.max_summary_sentences, 7)
        self.assertEqual(config.summary_soft_stop, 150)

    def test_claude_stop_honors_configured_sentence_limit(self):
        transcript = self.root / "limits.jsonl"
        transcript.write_text(
            json.dumps(
                {
                    "type": "assistant",
                    "message": {
                        "content": [{"type": "text", "text": "First one. Second one. Third one."}]
                    },
                }
            )
            + "\n"
        )
        spoken = []
        with patch.object(voice, "speak", side_effect=lambda text, *_a, **_k: spoken.append(text)):
            voice.handle_claude_stop(
                {"transcript_path": str(transcript), "session_id": "limits"},
                self.config(max_summary_sentences=1),
                background=False,
            )

        self.assertEqual(spoken, ["First one."])

    def test_transcribe_uses_whisper_and_cleans_timestamps(self):
        whisper = self.executable(
            "whisper-cli",
            "printf '[00:00:00.000 --> 00:00:01.000] Hello JSON world.\\n'",
        )
        audio = self.root / "audio.wav"
        audio.write_bytes(b"0" * 100)

        result = voice.transcribe(
            audio, self.config(whisper_command=whisper)
        )

        self.assertEqual(result, "Hello JSON world.")

    def test_speak_passes_summary_to_provider(self):
        output = self.root / "spoken.txt"
        speaker = self.executable(
            "say",
            'printf "%s\\n" "$@" > "$FAKE_SAY_OUTPUT"',
        )
        with patch.dict(os.environ, {"FAKE_SAY_OUTPUT": str(output)}):
            voice.speak(
                "# Done\nChanged the configuration. More details follow.",
                self.config(say_command=speaker),
                summary=True,
                background=False,
            )

        self.assertIn("Done. Changed the configuration.", output.read_text())

    def test_claude_stop_extracts_last_assistant_message(self):
        transcript = self.root / "session.jsonl"
        transcript.write_text(
            "\n".join(
                [
                    json.dumps(
                        {
                            "type": "assistant",
                            "message": {"content": [{"type": "text", "text": "Old response."}]},
                        }
                    ),
                    json.dumps(
                        {
                            "type": "assistant",
                            "message": {
                                "content": [
                                    {"type": "thinking", "thinking": "hidden"},
                                    {"type": "text", "text": "Current response."},
                                ]
                            },
                        }
                    ),
                ]
            )
            + "\n"
        )
        spoken = []
        with patch.object(voice, "speak", side_effect=lambda text, *_args, **_kwargs: spoken.append(text)):
            result = voice.handle_claude_stop(
                {"transcript_path": str(transcript), "session_id": "session"},
                self.config(),
                background=False,
            )

        self.assertTrue(result)
        self.assertEqual(spoken, ["Current response."])

    def test_claude_stop_prefers_final_message_over_lagging_transcript(self):
        transcript = self.root / "lagging.jsonl"
        transcript.write_text(
            json.dumps(
                {
                    "type": "assistant",
                    "message": {
                        "content": [
                            {"type": "text", "text": "Intermediate tool progress."}
                        ]
                    },
                }
            )
            + "\n"
        )
        spoken = []
        with patch.object(voice, "speak", side_effect=lambda text, *_a, **_k: spoken.append(text)):
            result = voice.handle_claude_stop(
                {
                    "transcript_path": str(transcript),
                    "session_id": "final-message",
                    "last_assistant_message": "This is the final visible response.",
                },
                self.config(),
                background=False,
            )

        self.assertTrue(result)
        self.assertEqual(spoken, ["This is the final visible response."])

    def test_cli_records_stops_and_transcribes(self):
        sox = self.executable(
            "sox",
            """
audio=""
for arg in "$@"; do
  case "$arg" in *.wav) audio="$arg" ;; esac
done
trap 'head -c 100 /dev/zero > "$audio"; exit 0' INT TERM
while true; do sleep 0.1; done
""",
        )
        whisper = self.executable("whisper-cli", "printf 'Recorded prompt.\\n'")
        cli = Path(__file__).parents[1] / "bin" / "voice"
        env = {
            **os.environ,
            "VOICE_STATE_DIR": str(self.root / "cli-state"),
            "VOICE_SOX_COMMAND": sox,
            "VOICE_WHISPER_COMMAND": whisper,
            "VOICE_WHISPER_MODEL": str(self.model),
        }

        started = subprocess.run([cli, "record"], env=env, capture_output=True, text=True)
        self.assertEqual(started.returncode, 0, started.stderr)
        time.sleep(0.2)
        stopped = subprocess.run([cli, "stop"], env=env, capture_output=True, text=True)

        self.assertEqual(stopped.returncode, 0, stopped.stderr)
        self.assertEqual(stopped.stdout.strip(), "Recorded prompt.")

    def test_record_reports_provider_that_exits_immediately(self):
        failed_sox = self.executable("failed-sox", "printf 'microphone denied\\n' >&2; exit 1")

        with self.assertRaisesRegex(voice.VoiceError, "microphone denied"):
            voice.start_recording(self.config(sox_command=failed_sox))


if __name__ == "__main__":
    unittest.main()

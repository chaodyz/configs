#!/usr/bin/env python3
"""Shared speech-to-text and text-to-speech core for local AI clients."""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import re
import shutil
import signal
import subprocess
import sys
import time
from dataclasses import dataclass
from pathlib import Path


class VoiceError(RuntimeError):
    pass


@dataclass(frozen=True)
class Config:
    state_dir: Path
    sox_command: str
    whisper_command: str
    whisper_model: Path
    say_command: str
    microphone: str | None
    voice_name: str | None
    voice_rate: int
    max_summary_chars: int
    claude_wait_seconds: float
    # Appended with defaults so existing positional/keyword construction keeps working.
    max_summary_sentences: int = 3
    # Stop collecting once the summary reaches this many chars; 0 disables it.
    # This was hard-coded to 120, which meant any opening sentence of 120+ chars
    # ended the summary at one sentence regardless of the other limits.
    summary_soft_stop: int = 0

    @classmethod
    def from_env(cls) -> "Config":
        home = Path.home()
        return cls(
            state_dir=Path(
                os.environ.get("VOICE_STATE_DIR", home / ".cache" / "voice-interface")
            ).expanduser(),
            sox_command=os.environ.get("VOICE_SOX_COMMAND", "sox"),
            whisper_command=os.environ.get("VOICE_WHISPER_COMMAND", "whisper-cli"),
            whisper_model=Path(
                os.environ.get(
                    "VOICE_WHISPER_MODEL",
                    home
                    / ".local"
                    / "share"
                    / "whisper-cpp"
                    / "ggml-large-v3-turbo-q5_0.bin",
                )
            ).expanduser(),
            say_command=os.environ.get("VOICE_SAY_COMMAND", "/usr/bin/say"),
            microphone=os.environ.get("VOICE_MICROPHONE") or None,
            voice_name=os.environ.get("VOICE_TTS_VOICE") or None,
            voice_rate=int(os.environ.get("VOICE_TTS_RATE", "210")),
            max_summary_chars=int(os.environ.get("VOICE_SUMMARY_CHARS", "400")),
            claude_wait_seconds=float(os.environ.get("VOICE_CLAUDE_WAIT_SECONDS", "6")),
            max_summary_sentences=int(os.environ.get("VOICE_SUMMARY_SENTENCES", "3")),
            summary_soft_stop=int(os.environ.get("VOICE_SUMMARY_SOFT_STOP", "0")),
        )

    @property
    def recording_path(self) -> Path:
        return self.state_dir / "recording.wav"

    @property
    def recording_pid_path(self) -> Path:
        return self.state_dir / "recording.pid"

    @property
    def recording_log_path(self) -> Path:
        return self.state_dir / "recording.log"

    @property
    def speech_pid_path(self) -> Path:
        return self.state_dir / "speech.pid"


def command_exists(command: str) -> bool:
    return Path(command).is_file() if "/" in command else shutil.which(command) is not None


def process_alive(pid: int) -> bool:
    try:
        os.kill(pid, 0)
        return True
    except (ProcessLookupError, PermissionError):
        return False


def read_pid(path: Path) -> int | None:
    try:
        value = path.read_text().strip()
        return int(value) if value else None
    except (FileNotFoundError, ValueError):
        return None


def start_recording(config: Config) -> int:
    if not command_exists(config.sox_command):
        raise VoiceError(f"recording command not found: {config.sox_command}")

    config.state_dir.mkdir(parents=True, exist_ok=True)
    previous_pid = read_pid(config.recording_pid_path)
    if previous_pid and process_alive(previous_pid):
        raise VoiceError(f"recording already active (pid {previous_pid})")

    config.recording_pid_path.unlink(missing_ok=True)
    config.recording_path.unlink(missing_ok=True)
    input_args = (
        ["-t", "coreaudio", config.microphone] if config.microphone else ["-d"]
    )
    args = [
        config.sox_command,
        *input_args,
        "-r",
        "16000",
        "-c",
        "1",
        "-b",
        "16",
        str(config.recording_path),
        "silence",
        "1",
        "0.1",
        "1%",
    ]
    log = config.recording_log_path.open("ab")
    try:
        process = subprocess.Popen(
            args,
            stdin=subprocess.DEVNULL,
            stdout=log,
            stderr=log,
            start_new_session=True,
        )
    finally:
        log.close()
    readiness_deadline = time.monotonic() + 0.5
    while (
        process.poll() is None
        and not config.recording_path.exists()
        and time.monotonic() < readiness_deadline
    ):
        time.sleep(0.02)
    if process.poll() is not None:
        detail = config.recording_log_path.read_text(errors="replace").strip().splitlines()
        raise VoiceError(detail[-1] if detail else "recording process exited immediately")
    config.recording_pid_path.write_text(f"{process.pid}\n")
    return process.pid


def stop_process(pid: int, timeout: float = 3) -> None:
    if not process_alive(pid):
        return
    try:
        os.kill(pid, signal.SIGINT)
    except ProcessLookupError:
        return
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        if not process_alive(pid):
            return
        time.sleep(0.1)
    try:
        os.kill(pid, signal.SIGKILL)
    except ProcessLookupError:
        pass


def cancel_recording(config: Config) -> None:
    pid = read_pid(config.recording_pid_path)
    if pid:
        stop_process(pid)
    config.recording_pid_path.unlink(missing_ok=True)
    config.recording_path.unlink(missing_ok=True)


def clean_transcription(text: str) -> str:
    text = re.sub(r"\[[0-9:.\- >]+\]", " ", text)
    return re.sub(r"\s+", " ", text).strip()


def transcribe(audio_path: Path, config: Config) -> str:
    if not command_exists(config.whisper_command):
        raise VoiceError(f"transcription command not found: {config.whisper_command}")
    if not config.whisper_model.is_file():
        raise VoiceError(f"Whisper model not found: {config.whisper_model}")
    if not audio_path.is_file() or audio_path.stat().st_size <= 44:
        raise VoiceError(f"recording is missing or empty: {audio_path}")

    result = subprocess.run(
        [
            config.whisper_command,
            "-m",
            str(config.whisper_model),
            "-f",
            str(audio_path),
            "-np",
            "-nt",
        ],
        capture_output=True,
        text=True,
        timeout=60,
        check=False,
    )
    if result.returncode != 0:
        detail = result.stderr.strip().splitlines()
        raise VoiceError(detail[-1] if detail else "transcription failed")
    text = clean_transcription(result.stdout)
    if not text:
        raise VoiceError("no speech detected")
    return text


def stop_and_transcribe(config: Config) -> str:
    pid = read_pid(config.recording_pid_path)
    if not pid:
        raise VoiceError("no recording is active")
    stop_process(pid)
    config.recording_pid_path.unlink(missing_ok=True)
    return transcribe(config.recording_path, config)


def summarize_for_speech(
    text: str,
    max_chars: int = 400,
    max_sentences: int = 3,
    soft_stop: int = 0,
) -> str:
    text = re.sub(r"```.*?```", " ", text, flags=re.DOTALL)
    text = re.sub(r"^#{1,6}\s+(.+?)\s*$", r"\1.", text, flags=re.MULTILINE)
    text = re.sub(r"^\s*(?:[-*+]|\d+\.)\s+", "", text, flags=re.MULTILINE)
    text = re.sub(r"\[([^]]*)\]\([^)]*\)", r"\1", text)
    text = re.sub(r"`([^`]*)`", r"\1", text)
    text = re.sub(r"[*_]{1,2}([^*_]+)[*_]{1,2}", r"\1", text)
    text = re.sub(r"[|>]", " ", text)
    text = re.sub(r"\s+", " ", text).strip()
    if not text:
        return ""

    sentences = re.findall(r".+?[.!?](?=\s|$)", text)
    selected: list[str] = []
    for sentence in sentences[:max_sentences]:
        sentence = sentence.strip()
        candidate = " ".join([*selected, sentence]).strip()
        if selected and len(candidate) > max_chars:
            break
        selected.append(sentence)
        if soft_stop > 0 and len(candidate) >= soft_stop:
            break
    summary = " ".join(selected) if selected else text
    if len(summary) > max_chars:
        # Cut on a word boundary so the last word is not spoken as a fragment.
        summary = summary[:max_chars].rsplit(" ", 1)[0]
    return summary.strip()


def summarize(text: str, config: Config) -> str:
    """Summarize using the limits from config, so every caller honors the knobs."""
    return summarize_for_speech(
        text,
        config.max_summary_chars,
        config.max_summary_sentences,
        config.summary_soft_stop,
    )


def stop_speaking(config: Config) -> None:
    pid = read_pid(config.speech_pid_path)
    if pid and process_alive(pid):
        try:
            os.kill(pid, signal.SIGTERM)
        except ProcessLookupError:
            pass
    config.speech_pid_path.unlink(missing_ok=True)


def speak(
    text: str, config: Config, *, summary: bool, background: bool
) -> subprocess.Popen:
    if not command_exists(config.say_command):
        raise VoiceError(f"speech command not found: {config.say_command}")
    spoken = summarize(text, config) if summary else text.strip()
    if not spoken:
        raise VoiceError("nothing speakable found")

    args = [config.say_command, "-r", str(config.voice_rate)]
    if config.voice_name:
        args.extend(["-v", config.voice_name])
    args.extend(["--", spoken])
    config.state_dir.mkdir(parents=True, exist_ok=True)
    stop_speaking(config)
    process = subprocess.Popen(
        args,
        stdin=subprocess.DEVNULL,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
        start_new_session=True,
    )
    config.speech_pid_path.write_text(f"{process.pid}\n")
    if background:
        return process
    returncode = process.wait()
    if read_pid(config.speech_pid_path) == process.pid:
        config.speech_pid_path.unlink(missing_ok=True)
    if returncode != 0 and returncode != -signal.SIGTERM:
        raise VoiceError(f"speech command exited {returncode}")
    return process


def extract_last_assistant_text(transcript_path: Path) -> str:
    latest = ""
    with transcript_path.open() as transcript:
        for line in transcript:
            try:
                item = json.loads(line)
            except json.JSONDecodeError:
                continue
            if item.get("type") != "assistant":
                continue
            content = item.get("message", {}).get("content", [])
            for part in content if isinstance(content, list) else []:
                if part.get("type") == "text" and part.get("text"):
                    latest = part["text"]
    return latest


def handle_claude_stop(payload: dict, config: Config, *, background: bool = True) -> bool:
    session_id = str(payload.get("session_id", "default"))
    state_path = config.state_dir / f"claude-last-{session_id}"
    config.state_dir.mkdir(parents=True, exist_ok=True)
    previous_hash = state_path.read_text().strip() if state_path.exists() else ""

    # Stop hooks provide the final rendered response directly. The transcript is
    # flushed asynchronously and can still end on an intermediate progress
    # message when the hook fires, so only parse it for older Claude versions.
    text = str(payload.get("last_assistant_message") or "").strip()
    if text:
        current_hash = hashlib.sha1(text.encode()).hexdigest()
        if current_hash == previous_hash:
            return False
    else:
        transcript_value = payload.get("transcript_path")
        if not transcript_value:
            return False
        transcript_path = Path(transcript_value).expanduser()
        if not transcript_path.is_file():
            return False
        deadline = time.monotonic() + config.claude_wait_seconds
        while True:
            text = extract_last_assistant_text(transcript_path)
            current_hash = hashlib.sha1(text.encode()).hexdigest() if text else ""
            if current_hash and current_hash != previous_hash:
                break
            if time.monotonic() >= deadline:
                return False
            time.sleep(0.2)

    spoken = summarize(text, config)
    if not spoken:
        return False
    state_path.write_text(f"{current_hash}\n")
    speak(spoken, config, summary=False, background=background)
    return True


def doctor(config: Config) -> dict:
    return {
        "recording": {
            "command": config.sox_command,
            "available": command_exists(config.sox_command),
        },
        "stt": {
            "provider": "whisper-cpp",
            "command": config.whisper_command,
            "command_available": command_exists(config.whisper_command),
            "model": str(config.whisper_model),
            "model_available": config.whisper_model.is_file(),
        },
        "tts": {
            "provider": "macos-say",
            "command": config.say_command,
            "available": command_exists(config.say_command),
        },
        "recording_active": bool(
            (pid := read_pid(config.recording_pid_path)) and process_alive(pid)
        ),
    }


def parser() -> argparse.ArgumentParser:
    result = argparse.ArgumentParser(description=__doc__)
    commands = result.add_subparsers(dest="command", required=True)
    commands.add_parser("record", help="Start recording from the configured microphone.")
    commands.add_parser("stop", help="Stop recording, transcribe, and print text.")
    commands.add_parser("cancel", help="Cancel and delete the active recording.")
    commands.add_parser("stop-speaking", help="Stop current speech playback.")
    transcribe_parser = commands.add_parser("transcribe", help="Transcribe an existing WAV file.")
    transcribe_parser.add_argument("audio", type=Path)
    speak_parser = commands.add_parser("speak", help="Speak arguments or stdin.")
    speak_parser.add_argument("text", nargs="*")
    speak_parser.add_argument("--summary", action="store_true")
    speak_parser.add_argument("--background", action="store_true")
    commands.add_parser("claude-stop", help="Handle a Claude Code Stop-hook JSON payload.")
    doctor_parser = commands.add_parser("doctor", help="Check provider dependencies.")
    doctor_parser.add_argument("--json", action="store_true")
    return result


def main(argv: list[str] | None = None) -> int:
    args = parser().parse_args(argv)
    config = Config.from_env()
    try:
        if args.command == "record":
            pid = start_recording(config)
            print(f"recording started (pid {pid})", file=sys.stderr)
        elif args.command == "stop":
            print(stop_and_transcribe(config))
        elif args.command == "cancel":
            cancel_recording(config)
            print("recording cancelled", file=sys.stderr)
        elif args.command == "stop-speaking":
            stop_speaking(config)
        elif args.command == "transcribe":
            print(transcribe(args.audio, config))
        elif args.command == "speak":
            text = " ".join(args.text) if args.text else sys.stdin.read()
            speak(text, config, summary=args.summary, background=args.background)
        elif args.command == "claude-stop":
            try:
                payload = json.load(sys.stdin)
                handle_claude_stop(payload, config)
            except (json.JSONDecodeError, VoiceError) as exc:
                print(f"voice: {exc}", file=sys.stderr)
            return 0
        elif args.command == "doctor":
            status = doctor(config)
            if args.json:
                print(json.dumps(status, indent=2, sort_keys=True))
            else:
                for section, value in status.items():
                    print(f"{section}: {value}")
            required = (
                status["recording"]["available"]
                and status["stt"]["command_available"]
                and status["stt"]["model_available"]
                and status["tts"]["available"]
            )
            return 0 if required else 1
    except (VoiceError, OSError, subprocess.SubprocessError) as exc:
        print(f"voice: {exc}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())

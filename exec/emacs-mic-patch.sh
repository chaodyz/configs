#!/usr/bin/env bash
# emacs-mic-patch — let terminal programs inside Emacs.app use the microphone
# (Claude Code dictation in a ghostel/eat/vterm buffer).
#
# Why this is needed:
#   macOS attributes microphone access to the host .app, not to the CLI running
#   inside it — so dictation in an Emacs terminal buffer needs the grant on
#   org.gnu.Emacs. The emacsformacosx.com (Galvanix) build ships with:
#     - no NSMicrophoneUsageDescription in Info.plist, so macOS can never show
#       the consent prompt and silently denies the request ("No audio detected
#       from microphone"), and the Microphone pane has no "+" to add it by hand;
#     - hardened runtime enabled but no com.apple.security.device.audio-input
#       entitlement.
#   This adds both, then re-signs ad-hoc (which is what lets the entitlement
#   change stick).
#
# Re-run after EVERY Emacs upgrade: replacing the .app restores the pristine
# Info.plist and Galvanix signature, undoing both patches. macOS will also
# re-prompt for the mic each time, since ad-hoc re-signing changes the cdhash
# that TCC keyed the old grant to.
#
# Trade-off: the app copy is no longer signed with Galvanix's Developer ID, so
# Gatekeeper treats it as locally signed and its other TCC grants (Desktop,
# Documents, Downloads, ...) need re-approving.
#
# Usage:
#   emacs-mic-patch.sh [--force] [/path/to/Emacs.app]
#
# Examples:
#   emacs-mic-patch.sh                      # patch /Applications/Emacs.app
#   emacs-mic-patch.sh --force              # re-sign even if already patched

set -euo pipefail

usage() {
  sed -n '/^# Usage:/,/^$/p' "$0" | sed 's/^# \{0,1\}//' >&2
  exit 1
}

force=0
app=/Applications/Emacs.app

while [[ $# -gt 0 ]]; do
  case "$1" in
    --force) force=1; shift ;;
    -h|--help) usage ;;
    -*) echo "unknown option: $1" >&2; usage ;;
    *) app="$1"; shift ;;
  esac
done

plist="$app/Contents/Info.plist"

[[ -d "$app" ]]    || { echo "not found: $app" >&2; exit 1; }
[[ -f "$plist" ]]  || { echo "not an app bundle (no Info.plist): $app" >&2; exit 1; }
[[ -w "$plist" ]]  || { echo "not writable (try sudo): $plist" >&2; exit 1; }

has_key() { /usr/libexec/PlistBuddy -c 'Print :NSMicrophoneUsageDescription' "$plist" >/dev/null 2>&1; }
has_ent() { codesign -d --entitlements - --xml "$app" 2>/dev/null | grep -q 'device\.audio-input'; }

if (( ! force )) && has_key && has_ent; then
  echo "already patched: $app" >&2
  echo "(nothing to do — pass --force to re-sign anyway)" >&2
  exit 0
fi

if pgrep -qf "$app/Contents/MacOS/Emacs"; then
  echo "! Emacs is running — quit it fully and relaunch after this finishes," >&2
  echo "  otherwise the running instance keeps the old signature." >&2
fi

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

backup="$work/Info.plist.bak"
cp "$plist" "$backup"

# 1. Usage description — without it macOS shows no prompt at all.
if has_key; then
  echo "→ NSMicrophoneUsageDescription already present" >&2
else
  /usr/libexec/PlistBuddy -c 'Add :NSMicrophoneUsageDescription string "Emacs requires permission to use the microphone for terminal programs such as Claude Code dictation."' "$plist"
  echo "→ added NSMicrophoneUsageDescription" >&2
fi

# 2. Entitlements — start from whatever the shipped build has so a future
#    Emacs release keeping extra entitlements does not lose them here.
ent="$work/entitlements.plist"
if ! codesign -d --entitlements - --xml "$app" 2>/dev/null > "$ent" || [[ ! -s "$ent" ]]; then
  echo "→ no existing entitlements found, starting fresh" >&2
  cat > "$ent" <<'PLIST'
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
</dict>
</plist>
PLIST
fi

/usr/libexec/PlistBuddy -c 'Print :com.apple.security.device.audio-input' "$ent" >/dev/null 2>&1 \
  || /usr/libexec/PlistBuddy -c 'Add :com.apple.security.device.audio-input bool true' "$ent"

# 3. Re-sign ad-hoc, keeping the hardened runtime.
echo "→ re-signing $app (ad-hoc)" >&2
if ! codesign --force --deep --sign - --options runtime --entitlements "$ent" "$app"; then
  echo "codesign failed — restoring original Info.plist" >&2
  cp "$backup" "$plist"
  echo "the bundle signature may now be inconsistent; reinstall Emacs to be safe" >&2
  exit 1
fi

# 4. Verify.
has_key || { echo "verify failed: NSMicrophoneUsageDescription missing" >&2; exit 1; }
has_ent || { echo "verify failed: audio-input entitlement missing" >&2; exit 1; }
codesign -v "$app" 2>&1 || { echo "verify failed: signature invalid" >&2; exit 1; }

echo >&2
echo "✓ patched and re-signed:" >&2
codesign -dv --verbose=2 "$app" 2>&1 | grep -E '^(Identifier|Authority|CodeDirectory)' | sed 's/^/    /' >&2
echo >&2
echo "Next: quit Emacs fully, relaunch, and trigger dictation — approve the" >&2
echo "microphone prompt when it appears. Confirm the grant landed with:" >&2
echo "    sqlite3 ~/Library/Application\\ Support/com.apple.TCC/TCC.db \\" >&2
echo "      \"select client,auth_value from access where service='kTCCServiceMicrophone';\"" >&2

#!/usr/bin/env bash

input=$(cat)

cwd=$(echo "$input" | jq -r '.workspace.current_dir // .cwd // empty')
dir=$(basename "$cwd")

# Git branch (skip optional locks)
git_branch=$(git -C "$cwd" --no-optional-locks branch --show-current 2>/dev/null)

# Model display name
model=$(echo "$input" | jq -r '.model.display_name // empty')

# Context usage
used_pct=$(echo "$input" | jq -r '.context_window.used_percentage // empty')

# Build output mirroring Starship style
output=""

# Directory - cyan bold
output+=$(printf '\033[1;36m%s\033[0m' "$dir")

# Git branch - green bold with Starship git symbol
if [ -n "$git_branch" ]; then
  output+=$(printf ' on \033[1;32m %s\033[0m' "$git_branch")
fi

# Model - dimmed magenta
if [ -n "$model" ]; then
  output+=$(printf ' \033[2;35m[%s]\033[0m' "$model")
fi

# Context window usage
if [ -n "$used_pct" ]; then
  used_int=${used_pct%.*}
  if [ "$used_int" -ge 80 ] 2>/dev/null; then
    output+=$(printf ' \033[1;31mctx:%s%%\033[0m' "$used_pct")
  else
    output+=$(printf ' \033[0;33mctx:%s%%\033[0m' "$used_pct")
  fi
fi

printf '%s' "$output"

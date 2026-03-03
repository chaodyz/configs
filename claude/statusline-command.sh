#!/usr/bin/env bash

# Claude Code statusline script.
# Receives a JSON payload via stdin on every turn and prints a single line
# that Claude Code renders in the status bar.

# ANSI color codes.
RESET=$'\033[0m'
CYAN=$'\033[36m'           # project name
ORANGE=$'\033[38;5;208m'   # sonnet model
RED=$'\033[31m'            # opus model / ctx >= 75%
YELLOW=$'\033[33m'         # haiku model / ctx >= 33%
GREEN=$'\033[32m'          # ctx < 33%

# Read the full JSON payload from stdin once so we can query it multiple times.
input=$(cat)

# Current project directory name (basename only).
project=$(echo "$input" | jq -r '.workspace.project_dir // .workspace.current_dir // ""')
project=$(basename "$project")

# Human-readable model name (e.g. "Claude Sonnet 4.6") and its API model ID
# (e.g. "claude-sonnet-4-6") used for pricing lookup and color selection below.
model=$(echo "$input" | jq -r '.model.display_name // ""')
model_id=$(echo "$input" | jq -r '.model.id // ""')

# Color the model name by family.
case "$model_id" in
  *opus*)  model_color=$RED    ;;
  *haiku*) model_color=$YELLOW ;;
  *)       model_color=$ORANGE ;;  # sonnet default
esac

# Context window utilisation as a rounded percentage.
used=$(echo "$input" | jq -r '.context_window.used_percentage // empty')
if [ -n "$used" ]; then
  ctx_num=$(printf "%.0f" "$used")
  ctx="${ctx_num}%"
else
  ctx_num=0
  ctx="0%"
fi

# Color the ctx percentage by threshold: green < 33%, yellow < 75%, red >= 75%.
if [ "$ctx_num" -lt 33 ]; then
  ctx_color=$GREEN
elif [ "$ctx_num" -lt 75 ]; then
  ctx_color=$YELLOW
else
  ctx_color=$RED
fi

# Per-turn token counts — reset each API call.
in_tokens=$(echo "$input" | jq -r '.context_window.current_usage.input_tokens // empty')
out_tokens=$(echo "$input" | jq -r '.context_window.current_usage.output_tokens // empty')

# Cumulative session token counts — grow throughout the conversation.
total_in=$(echo "$input" | jq -r '.context_window.total_input_tokens // empty')
total_out=$(echo "$input" | jq -r '.context_window.total_output_tokens // empty')

if [ -n "$in_tokens" ] && [ -n "$out_tokens" ]; then
  # Total tokens consumed this session (input + output combined).
  session_total=$(( ${total_in:-0} + ${total_out:-0} ))

  # Pricing per million tokens (USD). Rates are matched against the model ID;
  # sonnet is the default fallback.
  # To update prices, check https://www.anthropic.com/pricing
  case "$model_id" in
    *opus*)   in_price="5";  out_price="25" ;;
    *haiku*)  in_price="1";  out_price="5"  ;;
    *)        in_price="3";  out_price="15" ;;
  esac

  # Estimated session cost in USD using cumulative token counts.
  price=$(awk -v ti="${total_in:-0}" -v to="${total_out:-0}" \
              -v ip="$in_price" -v op="$out_price" \
              'BEGIN { printf "%.2f", (ti * ip + to * op) / 1000000 }')

  tokens="in: $in_tokens, out: $out_tokens, session: $session_total, \$: $price"
  echo "${CYAN}${project}${RESET} | ${model_color}${model}${RESET} | ctx: ${ctx_color}${ctx}${RESET} | $tokens"
else
  echo "${CYAN}${project}${RESET} | ${model_color}${model}${RESET} | ctx: ${ctx_color}${ctx}${RESET}"
fi

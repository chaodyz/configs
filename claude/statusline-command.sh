#!/usr/bin/env bash

input=$(cat)

project=$(echo "$input" | jq -r '.workspace.project_dir // .workspace.current_dir // ""')
project=$(basename "$project")

model=$(echo "$input" | jq -r '.model.display_name // ""')

used=$(echo "$input" | jq -r '.context_window.used_percentage // empty')
if [ -n "$used" ]; then
  ctx=$(printf "%.0f%%" "$used")
else
  ctx="0%"
fi

echo "$project | $model | ctx: $ctx"

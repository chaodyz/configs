Analyze the code the user is referring to — this could be a file, a specific function, or a range of lines.

## Instructions

1. Read the target (file, function, or line range) if not already in context.
2. Explain the code **line by line** or **block by block** depending on density:
   - For simple sequential code: go line by line
   - For dense/complex blocks: group 2-3 related lines and explain together
3. Structure the explanation as:
   - **What it does** — plain English description of the purpose
   - **Line-by-line breakdown** — explain each line or logical group
   - **Notable patterns or issues** — highlight anything worth attention (redundancy, potential bugs, conflicts, non-obvious behavior)

## Rules

- Do NOT suggest code changes unless the user asks
- Do NOT rewrite or refactor — only explain
- Use the file path and line numbers when referencing code (e.g. `org-visual.el:30`)
- Keep explanations concise but complete — avoid padding

## Arguments

`$ARGUMENTS` — can be any of:
- A file path: `/path/to/file.el`
- A function name: `my-org-mode-setup`
- A line range: `line 30-36`
- A combination: `my-org-mode-setup in org-visual.el`

If no argument is given, analyze the file currently open in the editor or ask the user to clarify.

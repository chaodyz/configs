Refine a project's CLAUDE.md to be lean and high-value for LLM context.

## Principles

- CLAUDE.md is for the LLM, not humans — keep only what helps the LLM make better decisions
- Less is more: if it can be derived from reading the code, don't include it
- No project overview, structure diagrams, module loading order, or "how to use" sections — that belongs in README.md
- Tools/configs: mention name and path only, no explanations
- Focus on: paths, naming conventions, architectural decisions that differ from defaults, and non-obvious constraints
- One sentence per topic is usually enough

## What belongs in CLAUDE.md

- Key paths the LLM needs to reference frequently
- Naming conventions and file organization patterns (e.g. "config split into {area}.el modules")
- Decisions that deviate from common defaults
- References to other docs for detailed info (e.g. `See @README.md for project overview`)
- Code style/conventions: include inline if short, otherwise delegate to a separate file and reference it (e.g. `For style conventions see @Style.md`). Same pattern applies to other lengthy topics — export to dedicated files and reference:
  - `For test strategy see @Test.md`
  - `For release plan see @Release.md`
  - This keeps CLAUDE.md as a lightweight index that points the LLM to detailed docs on demand

## What does NOT belong

- Project overview or description
- Directory tree / structure breakdown
- Explanations for well-known tools (e.g. `gh`, `glab`, `docker`). Internal or custom tools should get a brief description so the LLM knows what they do
- Behavioral rules already covered in `~/.claude/CLAUDE.md` (global instructions)
- Anything derivable from reading the code or git history

## Steps

1. Read the current CLAUDE.md (project-level) and ~/.claude/CLAUDE.md (global)
2. Identify what's redundant with global, what's derivable from code, and what's actually useful
3. Present findings to the user: what to keep, what to remove, what to move to README
4. Confirm with user before making changes
5. Rewrite CLAUDE.md — concise, one line per topic where possible
6. Move any stripped but still valuable info to README.md if it doesn't already exist there

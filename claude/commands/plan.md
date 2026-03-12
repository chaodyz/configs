You are helping plan implementation work. Follow these steps exactly — do NOT execute any code or make any file changes beyond writing the plan files.

## Step 1: Gather Context

If the user provided a ticket number (e.g. NOJ-1234, JRA-5678):
- Use the Jira MCP tool (`mcp__plugin_atlassian_atlassian__getJiraIssue`) to fetch the ticket
- Extract: title, description, acceptance criteria, priority, labels, linked issues

If no ticket number was given, use the user's description as the context.

## Step 2: Write Ticket Summary

Create the directory `./plan/` if it does not exist.

Write a file `./plan/TICKET.md` (or `./plan/TICKET-{NUMBER}.md` if a ticket number was given) with this structure:

```markdown
# {TICKET-NUMBER}: {Title}

## Summary
{1-3 sentence summary of what needs to be done and why}

## Acceptance Criteria
{List from ticket, or inferred from description}

## Context & Notes
{Any relevant background, linked issues, edge cases mentioned in the ticket}

## Scope
{What is explicitly in scope vs out of scope}
```

## Step 3: Write the Implementation Plan

Analyze the codebase as needed (read relevant files, search for patterns) to produce a concrete, actionable plan.

Write a file `./plan/PLAN.md` (or `./plan/PLAN-{NUMBER}.md` if a ticket number was given) with this structure:

```markdown
# Implementation Plan: {TICKET-NUMBER or short title}

## Root Cause / Problem Analysis
{What is causing the issue, or what gap exists}

## Approach
{High-level description of the solution strategy}

## Files to Modify
| File | Line(s) | Change |
|------|---------|--------|
| path/to/file.ts | 42 | Description of change |

## Implementation Steps
1. {Step one — specific, actionable}
2. {Step two}
...

## Verification
Steps to manually verify the fix works:
1. {How to test}
2. {What to look for}

## Risks & Considerations
{Any edge cases, potential regressions, or things to watch out for}
```

## Step 4: Stop

After writing both files, tell the user:
- The paths of the files written
- A one-line summary of the planned approach
- That they can review the files before proceeding

Do NOT implement anything. Do NOT run any commands. Wait for the user to review and give the go-ahead.

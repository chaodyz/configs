Create Jira tickets from an org file's `* Tickets` section and update the org file headings with ticket numbers.

## Step 1: Read the Org File

Read the org file provided by the user (or the one open in the IDE).

Parse the `* Tickets` section. For each `** heading`:
- **Ready**: no `TODO` keyword → create as a full ticket with description
- **Placeholder**: has `TODO` keyword → create with `[Placeholder]` prefix and minimal description
- **Skip**: already has a ticket number pattern like `[ABC-1234]` → skip, already created

> **Note:** `ABC` here represents the Jira project board key (e.g. `NOJ`, `LPCR`). Ask the user which project to use if not obvious from context.

## Step 2: Confirm the Plan with the User

Before creating anything, present a table:

| # | Heading | Type | Notes |
|---|---------|------|-------|
| 1 | Heading text | Ready / Placeholder | Any notes |

Ask the user to confirm:
1. The **Jira project key** (e.g. `NOJ`)
2. The **Epic** to link tickets to (search Jira first — ask user to confirm or create new)
3. The **assignee** (default: unassigned unless user specifies)
4. The **issue type** available in the project (check `getJiraProjectIssueTypesMetadata` — prefer Story, fallback to New Feature or Task)

Do NOT proceed until the user confirms.

## Step 3: Get Jira Context

Use `getAccessibleAtlassianResources` to get the `cloudId`.

Search for an existing Epic:
- Use `searchJiraIssuesUsingJql` with: `issuetype = Epic AND project = <PROJECT_KEY> AND summary ~ "<topic>"`
- Present results to user and ask: use existing epic, or create a new one?

If creating a new Epic, use `createJiraIssue` with `issueTypeName: "Epic"`.

Look up assignee account ID with `lookupJiraAccountId` if provided.

## Step 4: Create Tickets

For each **Ready** ticket:
- Use `createJiraIssue` with:
  - `issueTypeName`: confirmed type from Step 2
  - `assignee_account_id`: if provided
  - `contentFormat: "adf"`
  - `description`: structured ADF with headings and bullet lists built from the org subtree content under that heading

For each **Placeholder** ticket:
- Prepend `[Placeholder]` to the summary
- Use a minimal ADF description: "Placeholder ticket — pending UX/design review before this can be scoped."

**IMPORTANT — ADF format rules:**
- Use `{"type": "doc", "version": 1, "content": [...]}`
- Paragraphs: `{"type": "paragraph", "content": [{"type": "text", "text": "..."}]}`
- Headings: `{"type": "heading", "attrs": {"level": 3}, "content": [{"type": "text", "text": "..."}]}`
- Bullet lists: `{"type": "bulletList", "content": [{"type": "listItem", "content": [{"type": "paragraph", "content": [{"type": "text", "text": "..."}]}]}]}`
- Nested bullets: add a `bulletList` node inside a `listItem` after its `paragraph`
- Bold text: add `"marks": [{"type": "strong"}]` to a text node
- NEVER use `\n` escape sequences in text — use proper ADF nodes for line breaks

Create all ready tickets in parallel, then all placeholder tickets in parallel.

After creation, attempt to set the epic link via:
`editJiraIssue` with `fields: {"parent": {"key": "EPIC-KEY"}}`

## Step 5: Update the Org File

For each created ticket, edit the org file heading to prepend the ticket key:

- Ready tickets: `** [ABC-XXXX] Original Heading`
- Placeholder tickets: `** TODO [ABC-XXXX] Original Heading` (keep TODO keyword)

Make all edits in parallel.

## Step 6: Report

Print a summary table:

| Key | Title | Type | Link |
|-----|-------|------|------|
| ABC-XXXX | Title | Ready/Placeholder | URL |

Note any issues (e.g. epic link field not available on screen — manual linking required in Jira UI).

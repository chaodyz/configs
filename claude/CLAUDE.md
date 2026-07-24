## General
* Use brief, concise communication.
* Never make assumption without facts. No Hallucination.
* Never do code change without talking about the solution and explain why.
* Prefer .org for Personal document

## Tool
* Always use `gh` for Github operations
* Always use `glab` for Gitlab operations
* Always use `acli` for Atlassian JIRA operations, only use Atlasisan MCP for Confluence

## Path
* Dotfiles location: ~/projects/configs/ 
* Dotfiles Synchronization ~/projects/configs/exec/.*v2.sh
* Emacs configuration ~/projects/configs/emacs/
* General Knowledge base ~/eSync/org/* 

## Git Style

Commentary commit style. For instance
"{TICKET_NUM} [feat| fix | chore| ...]({SCOPE}): description about the change"

* the description should be concise, ideally in one sentence. 
* if there are multiple things, write the whole structure in another line with multi line style.
* no model signature at the end.

Branch name should follow {TICKET_NUMBER}/[feat|fix|hotfix|refactor|chore...]-description-in-kabeb

## Development Flow
Read ~/projects/configs/docs/workflow.md for Typical Agentic Dev Workflow.

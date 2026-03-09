## Dotfiles
- location: ~/projects/configs/
- description: The dotfiles projects helps to sync Tools(emacs,nvim, tmux, vscode, claude, joplin, alacritty etc.) configurations between machines. Under `./configs/exec/` there is restorev2 and backupv2 scripts that backup and restore files to actual locations and some config files are symlinked for version control.

## GitLab
- Always use `glab` CLI instead of GitLab MCP tools for all GitLab operations (MRs, issues, pipelines, API calls, etc.)

## Github
- Always use `gh` for Github operations

## Emacs
- I update regularly, consider use latest version references.
- Config location: `~/projects/configs/emacs/` this is a symlinked address, and init.el is loading .el files in different files. You may refer to `~/project/configs/emacs/config/` to see all the file 

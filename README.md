# Configs

Personal dotfiles managed via backup/restore scripts with optional symlink install.

## Structure

```
configs/
├── exec/              # Backup & restore scripts
├── emacs/             # Emacs config (modular: config/{area}.el)
├── nvim/              # Neovim config (VSCode-Neovim integration)
├── claude/            # Claude Code config & custom skills
├── vscode/            # VS Code keybindings (linux/mac)
├── cursor/            # Cursor IDE keybindings (linux/mac)
├── alacritty/         # Alacritty terminal config
├── tmux/              # Tmux config
├── starship/          # Starship prompt config
├── joplin/            # Joplin keymap
└── bash/              # Bash profile & rc files
```

## Usage

- **Backup**: `bash exec/backupv2.sh` — copies live configs into this repo
- **Restore**: `bash exec/restorev2.sh` — copies configs from repo to their expected locations
- **Symlink install**: Option in backupv2.sh to symlink configs instead of copying, keeping repo and live configs in sync

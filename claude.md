# Dotfiles Configuration Project

## Overview
This is a comprehensive dotfiles management system that syncs tool configurations across multiple machines. The project uses a backup/restore script system to manage configurations for development tools, editors, terminals, and productivity applications.

**Location**: `~/projects/configs/`

## Quick Start
```bash
# Backup current system configs to repo
~/projects/configs/exec/backupv2.sh

# Restore configs from repo to system
~/projects/configs/exec/restorev2.sh
```

## Project Structure
```
configs/
├── exec/              # Backup & restore scripts
│   ├── backupv2.sh    # Backup configs to repo
│   ├── restorev2.sh   # Restore configs from repo
│   ├── backup.sh      # Legacy backup script
│   ├── restore.sh     # Legacy restore script
│   ├── killport.sh    # Utility to kill process on port
│   └── mount.sh       # Mount helper
├── emacs/             # Emacs configuration (modular)
├── nvim/              # Neovim configuration
├── vscode/            # VS Code keybindings & settings
├── cursor/            # Cursor IDE keybindings
├── claude/            # Claude Code configuration
├── alacritty/         # Alacritty terminal config
├── tmux/              # Tmux configuration
├── starship/          # Starship prompt config
├── joplin/            # Joplin note-taking keymaps
└── bash/              # Bash profile & rc files
```

## Backup & Restore System

### Backup (backupv2.sh)
Copies configurations from system locations to the repo:
- Creates folder structure if missing
- Platform-specific handling (macOS/Linux)
- Uses rsync for complex directories (nvim)
- Backs up both files and directories
- Some configs are symlinked for live version control

### Restore (restorev2.sh)
Restores configurations from repo to system:
- Platform-aware restoration
- Creates necessary directories
- Handles both copy and symlink strategies
- Preserves permissions

### Symlinked Configs
Some configurations are symlinked (live updates):
- Check restorev2.sh for current symlink strategy
- Allows editing in-place with automatic version control

## Tool Configurations

### Emacs (`emacs/`)
**Location**: `~/projects/configs/emacs/` → `~/.emacs.d/`

Modular configuration with separate files for each functionality:

**Structure**:
- `init.el` - Bootstrap file that loads all modules
- `config/` - Individual module files

**Modules**:
- `base.el` - Package management (use-package, MELPA)
- `fonts.el` - Font configuration
- `theme.el` - Theme & circadian (time-based themes)
- `editing.el` - Evil mode (Vim keybindings), undo-tree
- `completion.el` - Ivy, Counsel, Company
- `keybindings.el` - General, which-key, hydra
- `org-core.el` - Org mode basics & babel
- `org-visual.el` - Org visual enhancements
- `org-workflow.el` - Agenda, TODO, capture templates
- `org-roam.el` - Zettelkasten note-taking (disabled)
- `development.el` - LSP (Eglot), treesitter, projectile, magit, markdown
- `chinese.el` - Pyim (拼音) input method
- `term-config.el` - Terminal integration
- `misc.el` - Helpful, restart-emacs, utilities

**Key Features**:
- Evil mode with Vim keybindings
- LSP support via Eglot
- Treesitter for syntax highlighting
- Projectile (searches `~/projects/` and `~/projects/backup/`)
- Magit for Git
- Ivy/Counsel completion framework
- Org mode with babel, agenda, capture
- Claude Code IDE integration (claude-code-ide.el)

**Module Loading Order**:
1. Core (base)
2. Visual (fonts, theme)
3. Editing (editing, completion, keybindings)
4. Org (org-core, org-visual, org-workflow)
5. Development (development)
6. Specialized (chinese, term-config, misc)

**Adding New Modules**:
1. Create `config/module-name.el`
2. Add `(provide 'module-name)` at end
3. Add `(require 'module-name)` in `init.el`

### Neovim (`nvim/`)
**Location**: `~/projects/configs/nvim/` → `~/.config/nvim/`

Modern Neovim configuration with Lazy.nvim plugin manager.

**Excluded from sync**:
- `lazy-lock.json`
- `plugin/packer_compiled.lua`
- `plugin/luacache*`

### VS Code (`vscode/`)
**Files**:
- `keybindings.mac.json` - macOS keybindings
- `keybindings.linux.json` - Linux keybindings

**System Locations**:
- macOS: `~/Library/Application Support/Code/User/`
- Linux: `~/.config/Code/User/`

### Cursor IDE (`cursor/`)
**Files**:
- `keybindings.mac.json` - macOS keybindings
- `keybindings.linux.json` - Linux keybindings

**System Locations**:
- macOS: `~/Library/Application Support/Cursor/User/`
- Linux: `~/.config/Cursor/User/`

### Claude Code (`claude/`)
**Location**: `~/projects/configs/claude/` → `~/.claude/`

Claude Code CLI configuration:
- `CLAUDE.md` - User-specific instructions and context
- `settings.json` - Claude Code settings
- `commands/` - Custom commands/skills

**Purpose**:
- Provides context about projects and preferences
- Configures Claude Code behavior
- Stores custom commands and workflows

### Terminal Tools

#### Alacritty (`alacritty/`)
**Location**: `~/.config/alacritty/alacritty.toml`

GPU-accelerated terminal emulator configuration.

#### Tmux (`tmux/`)
**Location**: `~/.tmux.conf`

Terminal multiplexer configuration with custom keybindings and plugins.

#### Starship (`starship/`)
**Location**: `~/.config/starship.toml`

Cross-shell prompt configuration with custom styling and modules.

#### Bash (`bash/`)
**Files**:
- `.bash_profile` (macOS only)
- `.bashrc`

**System Locations**: `~/`

### Joplin (`joplin/`)
**Location**: `~/.config/joplin/keymap.json`

Note-taking application keyboard shortcuts.

## Platform-Specific Behavior

### macOS
- Backs up `.bash_profile` and `.bashrc`
- Uses `~/Library/Application Support/` for GUI apps
- Handles Code and Cursor configs

### Linux
- Backs up only `.bashrc`
- Uses `~/.config/` for configs
- Handles Code and Cursor configs

## Git Version Control

**Repository**: `~/projects/configs/.git`

**Current Status** (from git status):
```
Modified:
- claude/CLAUDE.md
- claude/settings.json
- emacs/config/development.el
- emacs/config/keybindings.el

Untracked:
- emacs/claude.md
```

**Branches**:
- `main` - primary branch

**Recent Commits**:
- Adjust text scale
- Add claude config
- Update backup script
- Fix emacs syntax
- Refactor with claude code

## Maintenance Workflows

### Regular Backup
```bash
cd ~/projects/configs
./exec/backupv2.sh
git add .
git commit -m "chore: update configs"
git push
```

### Setting Up New Machine
```bash
git clone <repo-url> ~/projects/configs
cd ~/projects/configs
./exec/restorev2.sh
```

### Updating Single Tool
```bash
# Make changes to config file in ~/projects/configs/
./exec/restorev2.sh  # Apply changes
# Test changes
./exec/backupv2.sh   # Backup if satisfied
```

### Adding New Tool Config
1. Create directory: `mkdir ~/projects/configs/newtool`
2. Add backup logic to `exec/backupv2.sh`
3. Add restore logic to `exec/restorev2.sh`
4. Update this documentation

## Emacs Integration

### Claude Code IDE
This repo includes `claude-code-ide.el` integration which provides:
- MCP server access (emacs-tools)
- LSP integration (xref, references)
- Tree-sitter syntax information
- Project context awareness
- Diagnostics (flycheck/flymake)

**Coordinates**: Emacs uses mixed coordinates
- Lines: 1-based (line 1 = first line)
- Columns: 0-based (column 0 = first character)

## Tips & Best Practices

1. **Before Major Changes**: Run `backupv2.sh` to ensure current configs are saved
2. **Test on Non-Critical Machine First**: When restoring to new machine
3. **Review Diffs**: Use `git diff` before committing to catch unintended changes
4. **Document Custom Configs**: Add notes for non-obvious customizations
5. **Keep Scripts Updated**: When adding new tools, update both backup and restore scripts
6. **Check Platform**: Some configs are platform-specific (mac vs linux)

## Utilities

### killport.sh
Kill process running on a specific port:
```bash
./exec/killport.sh <port>
```

## Related Files
- `.gitignore` - Excludes temporary files and sensitive data
- `README.md` - Basic project description (outdated, this file is more current)

## Common Tasks

### Sync Emacs Config
```bash
cd ~/projects/configs
# Make changes to emacs/*.el files
./exec/backupv2.sh  # If editing in ~/.emacs.d/
# or just commit if editing in ~/projects/configs/emacs/
git commit -am "feat: update emacs config"
```

### Update Claude Instructions
```bash
# Edit claude/CLAUDE.md
# Changes take effect immediately (symlinked)
git commit claude/CLAUDE.md -m "docs: update claude context"
```

### Sync All Configs
```bash
./exec/backupv2.sh && git add . && git commit -m "chore: sync all configs"
```

## Notes
- Some undo-tree files (`.*~undo-tree~`) are Git-ignored
- Lock files (`.#filename`) are temporary and ignored
- Custom Emacs settings are stored in `init.el` via Custom interface
- Projectile searches: `~/projects/` and `~/projects/backup/`

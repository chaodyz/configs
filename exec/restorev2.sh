#!/usr/bin/env bash

set -euo pipefail

DOTFILES_DIR=~/projects/configs

apply_from_repo() {
    echo "Applying repo configs from $DOTFILES_DIR to local machine..."

    # --- Shell configs ---
    if [[ "$(uname)" == "Darwin" ]]; then
        cp "$DOTFILES_DIR/bash/.bash_profile" ~/.bash_profile
        cp "$DOTFILES_DIR/bash/.bashrc" ~/.bashrc
    elif [[ "$(uname)" == "Linux" ]]; then
        cp "$DOTFILES_DIR/bash/.bashrc" ~/.bashrc
    fi

    # --- Common configs ---
    cp "$DOTFILES_DIR/tmux/.tmux.conf" ~/.tmux.conf
    mkdir -p ~/.config
    cp "$DOTFILES_DIR/starship/starship.toml" ~/.config/starship.toml
    mkdir -p ~/.config/alacritty
    cp "$DOTFILES_DIR/alacritty/alacritty.toml" ~/.config/alacritty/alacritty.toml
    mkdir -p ~/.config/joplin
    cp "$DOTFILES_DIR/joplin/keymap.json" ~/.config/joplin/keymap.json

    # --- Neovim ---
    if [ -e ~/.config/nvim ] && [ ! -L ~/.config/nvim ]; then
        rm -rf ~/.config/nvim.bak
        mv ~/.config/nvim ~/.config/nvim.bak
        echo "Backed up existing ~/.config/nvim to ~/.config/nvim.bak"
    else
        rm -rf ~/.config/nvim/
    fi
    cp -rf "$DOTFILES_DIR/nvim" ~/.config/

    # --- Emacs ---
    mkdir -p ~/.emacs.d
    cp "$DOTFILES_DIR/emacs/init.el" ~/.emacs.d/init.el
    [[ -f "$DOTFILES_DIR/emacs/early-init.el" ]] && cp "$DOTFILES_DIR/emacs/early-init.el" ~/.emacs.d/early-init.el

    # Restore emacs config directory
    if [ -d "$DOTFILES_DIR/emacs/config" ]; then
        rm -rf ~/.emacs.d/config
        cp -rf "$DOTFILES_DIR/emacs/config" ~/.emacs.d/
    fi

    # --- Claude ---
    mkdir -p ~/.claude
    cp "$DOTFILES_DIR/claude/CLAUDE.md" ~/.claude/CLAUDE.md
    [[ -f "$DOTFILES_DIR/claude/settings.json" ]] && cp "$DOTFILES_DIR/claude/settings.json" ~/.claude/settings.json
    [[ -f "$DOTFILES_DIR/claude/statusline-command.sh" ]] && cp "$DOTFILES_DIR/claude/statusline-command.sh" ~/.claude/statusline-command.sh
    if [ -d "$DOTFILES_DIR/claude/commands" ]; then
        rm -rf ~/.claude/commands
        cp -rf "$DOTFILES_DIR/claude/commands" ~/.claude/
    fi

    # --- VSCode & Cursor (per platform) ---
    if [[ "$(uname)" == "Darwin" ]]; then
        mkdir -p "$HOME/Library/Application Support/Code/User"
        cp "$DOTFILES_DIR/vscode/keybindings.mac.json" \
           "$HOME/Library/Application Support/Code/User/keybindings.json"

        mkdir -p "$HOME/Library/Application Support/Cursor/User"
        cp "$DOTFILES_DIR/cursor/keybindings.mac.json" \
           "$HOME/Library/Application Support/Cursor/User/keybindings.json" 2>/dev/null || true

    elif [[ "$(uname)" == "Linux" ]]; then
        mkdir -p "$HOME/.config/Code/User"
        cp "$DOTFILES_DIR/vscode/keybindings.linux.json" \
           "$HOME/.config/Code/User/keybindings.json"

        mkdir -p "$HOME/.config/Cursor/User"
        cp "$DOTFILES_DIR/cursor/keybindings.linux.json" \
           "$HOME/.config/Cursor/User/keybindings.json" 2>/dev/null || true
    fi

    echo "Apply complete."
}

read -p "Apply repo configs to this machine? This will overwrite local configs. (y/n) " choice
case "$choice" in
    y|Y ) echo "Proceeding..."
          apply_from_repo ;;
    n|N ) echo "Exiting without changes." && exit ;;
    * ) echo "Invalid choice. Exiting." && exit ;;
esac

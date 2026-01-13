#!/usr/bin/env bash

DOTFILES_DIR=~/projects/configs

restore_action() {
    echo "Restoring config files from $DOTFILES_DIR..."

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
    rm -rf ~/.config/nvim/
    cp -rf "$DOTFILES_DIR/nvim" ~/.config/

    # --- Emacs ---
    mkdir -p ~/.emacs.d
    cp "$DOTFILES_DIR/emacs/init.el" ~/.emacs.d/init.el

    # Restore emacs config directory
    if [ -d "$DOTFILES_DIR/emacs/config" ]; then
        rm -rf ~/.emacs.d/config
        cp -rf "$DOTFILES_DIR/emacs/config" ~/.emacs.d/
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

    echo "Restore complete."
}

read -p "Do you want to proceed with restore? This will overwrite your local configs. (y/n) " choice
case "$choice" in
    y|Y ) echo "Proceeding..."
          restore_action ;;
    n|N ) echo "Exiting without changes." && exit ;;
    * ) echo "Invalid choice. Exiting." && exit ;;
esac

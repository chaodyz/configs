#!/usr/bin/env bash

DOTFILES_DIR=~/projects/configs

restore_action() {
    echo "♻ Restoring configs from $DOTFILES_DIR"

    # Platform specific
    if [[ "$(uname)" == "Darwin" ]]; then
        cp "$DOTFILES_DIR/.bash_profile" ~/.bash_profile
        cp "$DOTFILES_DIR/.bashrc" ~/.bashrc
        cp "$DOTFILES_DIR/vscode/keybindings.mac.json" \
           "$HOME/Library/Application Support/Code/User/keybindings.json"
        [[ -f "$DOTFILES_DIR/cursor/keybindings.mac.json" ]] && \
            cp "$DOTFILES_DIR/cursor/keybindings.mac.json" \
               "$HOME/Library/Application Support/Cursor/User/keybindings.json"
    elif [[ "$(uname)" == "Linux" ]]; then
        cp "$DOTFILES_DIR/.bashrc" ~/.bashrc
        cp "$DOTFILES_DIR/vscode/keybindings.linux.json" \
           "$HOME/.config/Code/User/keybindings.json"
        [[ -f "$DOTFILES_DIR/cursor/keybindings.linux.json" ]] && \
            cp "$DOTFILES_DIR/cursor/keybindings.linux.json" \
               "$HOME/.config/Cursor/User/keybindings.json"
    fi

    # Common configs
    cp "$DOTFILES_DIR/tmux/.tmux.conf" ~/.tmux.conf
    cp "$DOTFILES_DIR/starship/starship.toml" ~/.config/starship.toml
    cp "$DOTFILES_DIR/alacritty/alacritty.toml" ~/.config/alacritty/alacritty.toml
    cp "$DOTFILES_DIR/joplin/keymap.json" ~/.config/joplin/keymap.json
    rm -rf ~/.config/nvim/
    cp -rf "$DOTFILES_DIR/nvim" ~/.config/
    cp "$DOTFILES_DIR/emacs/init.el" ~/.emacs.d/init.el
    cp "$DOTFILES_DIR/emacs/emacs.org" ~/eSync/org/emacs.org

    echo "✅ Restore complete."
}

read -p "Do you want to restore your configs from dotfiles? (y/n) " choice

case "$choice" in
    y|Y ) restore_action ;;
    n|N ) echo "Exiting." ;;
    * ) echo "Invalid choice. Exiting." ;;
esac

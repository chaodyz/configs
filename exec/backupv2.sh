#!/usr/bin/env bash

DOTFILES_DIR=~/projects/configs

# ----------------------------------------------------------
# Backup function
# ----------------------------------------------------------
backup_action() {
    echo "📦 Backing up current config to: $DOTFILES_DIR"

    # Ensure folder structure exists
    mkdir -p "$DOTFILES_DIR"/{vscode,cursor,starship,alacritty,emacs,nvim,joplin,tmux}

    # Platform specific
    if [[ "$(uname)" == "Darwin" ]]; then
        cp ~/.bash_profile "$DOTFILES_DIR/.bash_profile"
        cp "$HOME/Library/Application Support/Code/User/keybindings.json" \
           "$DOTFILES_DIR/vscode/keybindings.mac.json"
        [[ -f "$HOME/Library/Application Support/Cursor/User/keybindings.json" ]] && \
            cp "$HOME/Library/Application Support/Cursor/User/keybindings.json" \
               "$DOTFILES_DIR/cursor/keybindings.mac.json"
    elif [[ "$(uname)" == "Linux" ]]; then
        cp ~/.bashrc "$DOTFILES_DIR/.bashrc"
        cp "$HOME/.config/Code/User/keybindings.json" \
           "$DOTFILES_DIR/vscode/keybindings.linux.json"
        [[ -f "$HOME/.config/Cursor/User/keybindings.json" ]] && \
            cp "$HOME/.config/Cursor/User/keybindings.json" \
               "$DOTFILES_DIR/cursor/keybindings.linux.json"
    fi

    # Common configs
    cp ~/.tmux.conf "$DOTFILES_DIR/tmux/.tmux.conf"
    cp ~/.config/starship.toml "$DOTFILES_DIR/starship/starship.toml"
    cp ~/.config/alacritty/alacritty.toml "$DOTFILES_DIR/alacritty/alacritty.toml"
    cp ~/.config/joplin/keymap.json "$DOTFILES_DIR/joplin/keymap.json"
    cp -rf ~/.config/nvim "$DOTFILES_DIR/nvim"
    cp ~/.emacs.d/init.el "$DOTFILES_DIR/emacs/init.el"
    cp ~/eSync/org/emacs.org "$DOTFILES_DIR/emacs/emacs.org"

    echo "✅ Backup complete."
}

# ----------------------------------------------------------
# Symlink install function
# ----------------------------------------------------------
install_symlinks() {
    echo "🔗 Installing symlinks from: $DOTFILES_DIR"

    # Backup old files if they are not symlinks
    backup_if_not_symlink() {
        local target="$1"
        if [[ -f "$target" && ! -L "$target" ]]; then
            mv "$target" "$target.bak"
            echo "💾 Backed up $target to $target.bak"
        fi
    }

    # Shell configs
    backup_if_not_symlink ~/.bashrc
    [[ -f "$DOTFILES_DIR/.bashrc" ]] && ln -sf "$DOTFILES_DIR/.bashrc" ~/.bashrc

    if [[ "$(uname)" == "Darwin" ]]; then
        backup_if_not_symlink ~/.bash_profile
        [[ -f "$DOTFILES_DIR/.bash_profile" ]] && \
            ln -sf "$DOTFILES_DIR/.bash_profile" ~/.bash_profile
    fi

    # Common configs
    ln -sf "$DOTFILES_DIR/tmux/.tmux.conf" ~/.tmux.conf
    ln -sf "$DOTFILES_DIR/starship/starship.toml" ~/.config/starship.toml
    ln -sf "$DOTFILES_DIR/alacritty/alacritty.toml" ~/.config/alacritty/alacritty.toml
    ln -sf "$DOTFILES_DIR/joplin/keymap.json" ~/.config/joplin/keymap.json
    ln -sf "$DOTFILES_DIR/nvim" ~/.config/nvim
    ln -sf "$DOTFILES_DIR/emacs/init.el" ~/.emacs.d/init.el
    ln -sf "$DOTFILES_DIR/emacs/emacs.org" ~/eSync/org/emacs.org

    # VSCode & Cursor configs
    if [[ "$(uname)" == "Darwin" ]]; then
        ln -sf "$DOTFILES_DIR/vscode/keybindings.mac.json" \
               "$HOME/Library/Application Support/Code/User/keybindings.json"
        [[ -f "$DOTFILES_DIR/cursor/keybindings.mac.json" ]] && \
            ln -sf "$DOTFILES_DIR/cursor/keybindings.mac.json" \
                   "$HOME/Library/Application Support/Cursor/User/keybindings.json"
    elif [[ "$(uname)" == "Linux" ]]; then
        ln -sf "$DOTFILES_DIR/vscode/keybindings.linux.json" \
               "$HOME/.config/Code/User/keybindings.json"
        [[ -f "$DOTFILES_DIR/cursor/keybindings.linux.json" ]] && \
            ln -sf "$DOTFILES_DIR/cursor/keybindings.linux.json" \
                   "$HOME/.config/Cursor/User/keybindings.json"
    fi

    echo "✅ Symlink install complete."
}

# ----------------------------------------------------------
# Menu
# ----------------------------------------------------------
read -p "Backup (b) or Install (i)? " choice

case "$choice" in
    b|B ) backup_action ;;
    i|I ) install_symlinks ;;
    * ) echo "❌ Invalid choice." && exit 1 ;;
esac

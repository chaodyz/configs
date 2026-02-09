#!/usr/bin/env bash

DOTFILES_DIR=~/projects/configs

# ----------------------------------------------------------
# Backup function
# ----------------------------------------------------------
backup_action() {
    echo "📦 Backing up current config to: $DOTFILES_DIR"

    # Ensure folder structure exists
    mkdir -p "$DOTFILES_DIR"/{vscode,cursor,starship,alacritty,emacs,nvim,joplin,tmux,claude}

    # Platform specific
    if [[ "$(uname)" == "Darwin" ]]; then

        cp ~/.bash_profile "$DOTFILES_DIR/bash/.bash_profile"
        cp ~/.bashrc "$DOTFILES_DIR/bash/.bashrc"

        cp "$HOME/Library/Application Support/Code/User/keybindings.json" \
           "$DOTFILES_DIR/vscode/keybindings.mac.json"
        [[ -f "$HOME/Library/Application Support/Cursor/User/keybindings.json" ]] && \
            cp "$HOME/Library/Application Support/Cursor/User/keybindings.json" \
               "$DOTFILES_DIR/cursor/keybindings.mac.json"
    elif [[ "$(uname)" == "Linux" ]]; then
        cp ~/.bashrc "$DOTFILES_DIR/bash/.bashrc"
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

    if [ -d "$HOME/.config/nvim" ]; then
        rsync -av --exclude 'lazy-lock.json' \
                  --exclude 'plugin/packer_compiled.lua' \
                  --exclude 'plugin/luacache*' \
            "$HOME/.config/nvim/" "$DOTFILES_DIR/nvim"
    fi

    cp ~/.emacs.d/init.el "$DOTFILES_DIR/emacs/init.el"

    # Backup emacs config directory
    if [ -d "$HOME/.emacs.d/config" ]; then
        rsync -av "$HOME/.emacs.d/config/" "$DOTFILES_DIR/emacs/config"
    fi

    # Claude configs
    cp ~/.claude/CLAUDE.md "$DOTFILES_DIR/claude/CLAUDE.md"
    [[ -f ~/.claude/settings.json ]] && cp ~/.claude/settings.json "$DOTFILES_DIR/claude/settings.json"
    if [ -d "$HOME/.claude/commands" ]; then
        rsync -av "$HOME/.claude/commands/" "$DOTFILES_DIR/claude/commands"
    fi

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
    echo "🔗 Symlinking shell configs ..."
    backup_if_not_symlink ~/.bashrc
    [[ -f "$DOTFILES_DIR/bash/.bashrc" ]] && ln -sf "$DOTFILES_DIR/bash/.bashrc" ~/.bashrc

    if [[ "$(uname)" == "Darwin" ]]; then
        backup_if_not_symlink ~/.bash_profile
        [[ -f "$DOTFILES_DIR/bash/.bash_profile" ]] && \
            ln -sf "$DOTFILES_DIR/bash/.bash_profile" ~/.bash_profile
    fi

    # Common configs
    echo "🔗 Symlinking common configs ..."
    ln -sf "$DOTFILES_DIR/tmux/.tmux.conf" ~/.tmux.conf
    ln -sf "$DOTFILES_DIR/starship/starship.toml" ~/.config/starship.toml
    ln -sf "$DOTFILES_DIR/alacritty/alacritty.toml" ~/.config/alacritty/alacritty.toml
    ln -sf "$DOTFILES_DIR/joplin/keymap.json" ~/.config/joplin/keymap.json

    # Emacs config
    echo "🔗 Symlinking Emacs config ..."
    ln -sf "$DOTFILES_DIR/emacs/init.el" ~/.emacs.d/init.el
    if [ -e "$HOME/.emacs.d/config" ] || [ -L "$HOME/.emacs.d/config" ]; then
        rm -rf "$HOME/.emacs.d/config"
    fi
    ln -s "$DOTFILES_DIR/emacs/config" "$HOME/.emacs.d/config"

    # Claude configs
    echo "🔗 Symlinking Claude configs ..."
    ln -sf "$DOTFILES_DIR/claude/CLAUDE.md" ~/.claude/CLAUDE.md
    [[ -f "$DOTFILES_DIR/claude/settings.json" ]] && \
        ln -sf "$DOTFILES_DIR/claude/settings.json" ~/.claude/settings.json
    if [ -e "$HOME/.claude/commands" ] || [ -L "$HOME/.claude/commands" ]; then
        rm -rf "$HOME/.claude/commands"
    fi
    ln -s "$DOTFILES_DIR/claude/commands" "$HOME/.claude/commands"

    # Neovim config
    echo "🔗 Symlinking Neovim config ..."
    if [ -e "$HOME/.config/nvim" ] || [ -L "$HOME/.config/nvim" ]; then
        rm -rf "$HOME/.config/nvim"
    fi
    ln -s "$DOTFILES_DIR/nvim" "$HOME/.config/nvim"

    # VSCode & Cursor configs
    echo "🔗 Symlinking VSCode & Cursor configs ..."
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
read -p "Backup (b) or Install Symlinks (i)? " choice

case "$choice" in
    b|B ) backup_action ;;
    i|I ) install_symlinks ;;
    * ) echo "❌ Invalid choice." && exit 1 ;;
esac

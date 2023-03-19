#!/usr/bin/env bash

echo "Backing up"

# Platform specific 
if [ "$(uname)" == "Darwin" ]; then
    # when its macos we copy both bashrc and profile
    cp ~/.bash_profile ~/projects/configs/.bash_profile &&
    cp ~/.bashrc ~/projects/configs/.bashrc

elif  [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]; then
    # Do something under GNU/Linux platform
    cp ~/.bashrc ~/projects/configs/.bashrc
      
# elif [ "$(expr substr $(uname -s) 1 10)" == "MINGW64_NT" ]; then
    # Do something under 32 bits Windows NT platform
    # else [ "$(expr substr $(uname -s) 1 10)" == "MINGW32_NT" ]; 
    # Do something under 64 bits Windows NT platform
fi

cp ~/.tmux.conf  ~/projects/configs/.tmux.conf &&

cp ~/.config/starship.toml  ~/projects/configs/starship.toml &&

cp ~/.config/alacritty/alacritty.yml  ~/projects/configs/alacritty.yml &&

cp ~/.config/joplin/keymap.json  ~/projects/configs/keymap.json &&

cp -rf ~/.config/nvim  ~/projects/configs/ &&

cp ~/.emacs.d/init.el ~/projects/configs/emacs/init.el &&

# backup org files
cp ~/eSync/org/emacs.org ~/projects/configs/emacs/emacs.org

echo "done"

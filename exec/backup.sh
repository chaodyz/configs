#!/usr/bin/env bash

if [ "$(uname)" == "Darwin" ]; then
    # Do something under Mac OS X platform        
cp ~/.bash_profile ~/projects/configs/.bash_profile &&
cp ~/.config/starship.toml ~/projects/configs/starship.toml &&
cp ~/.config/alacritty/alacritty.yml ~/projects/configs/alacritty.yml &&
cp ~/.config/joplin/keymap.json ~/projects/configs/keymap.json &&
cp ~/.tmux.conf ~/projects/configs/.tmux.conf 

elif  [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]; then
    # Do something under GNU/Linux platform
cp ~/.bashrc ~/projects/configs/.bashrc &&
cp ~/.config/starship.toml ~/projects/configs/starship.toml &&
cp ~/.config/alacritty/alacritty.yml ~/projects/configs/alacritty.yml &&
cp ~/.config/joplin/keymap.json ~/projects/configs/keymap.json &&
cp ~/.tmux.conf ~/projects/configs/.tmux.conf 
# elif [ "$(expr substr $(uname -s) 1 10)" == "MINGW32_NT" ]; then
    # Do something under 32 bits Windows NT platform
    # else [ "$(expr substr $(uname -s) 1 10)" == "MINGW64_NT" ]; 
    # Do something under 64 bits Windows NT platform
fi


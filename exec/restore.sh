#!/usr/bin/env bash

echo "Restoring"

# Platform specific
if [ "$(uname)" == "Darwin" ]; then
   # Do something under Mac OS X platform       
   cp ~/projects/configs/.bash_profile       ~/.bash_profile

elif  [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]; then
   # Do something under GNU/Linux platform
   cp ~/projects/configs/.bashrc     ~/.bashrc
    
# elif [ "$(expr substr $(uname -s) 1 10)" == "MINGW64_NT" ]; then
   # Do something under 32 bits Windows NT platform
   # else [ "$(expr substr $(uname -s) 1 10)" == "MINGW32_NT" ];
   # Do something under 64 bits Windows NT platform
fi

cp      ~/projects/configs/.tmux.conf             ~/.tmux.conf  &&
cp      ~/projects/configs/starship.toml          ~/.config/starship.toml  &&
cp      ~/projects/configs/alacritty.yml          ~/.config/alacritty/alacritty.yml  &&
cp      ~/projects/configs/keymap.json            ~/.config/joplin/keymap.json &&
rm -rf ~/.config/nvim/ &&
cp -rf  ~/projects/configs/nvim                   ~/.config/

echo "done"

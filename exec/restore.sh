#!/usr/bin/env bash

echo "Restoring"

dir_source = "~/projects/configs/"
dir_destination = "~/.config/"

# Platform specific 
if [ "$(uname)" == "Darwin" ]; then
    # Do something under Mac OS X platform        
    cp ${dir_source}.bash_profile       ~/.bash_profile

elif  [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]; then
    # Do something under GNU/Linux platform
    cp ${dir_source}.bashrc     ~/.bashrc
      
# elif [ "$(expr substr $(uname -s) 1 10)" == "MINGW64_NT" ]; then
    # Do something under 32 bits Windows NT platform
    # else [ "$(expr substr $(uname -s) 1 10)" == "MINGW32_NT" ]; 
    # Do something under 64 bits Windows NT platform
fi

cp      ${dir_source}.tmux.conf             ~/.tmux.conf &&
cp      ${dir_source}starship.toml          ${dir_destination}starship.toml  &&
cp      ${dir_source}alacritty.yml          ${dir_destination}alacritty/alacritty.yml  &&
cp      ${dir_source}keymap.json            ${dir_destinatin}joplin/keymap.json &&
cp -rf  ${dir_source}nvim                   ${dir_destination}nvim

echo "done"

#!/usr/bin/env bash

function restore_action() {
    echo "Restoring"
    # Platform specific
    if [ "$(uname)" == "Darwin" ]; then
	# Do something under Mac OS X platform       
	cp ~/projects/configs/.bash_profile       ~/.bash_profile &&
	    cp ~/projects/configs/.bashrc      ~/.bashrc

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
	cp      ~/projects/configs/alacritty.toml         ~/.config/alacritty/alacritty.toml  &&
	cp      ~/projects/configs/keymap.json            ~/.config/joplin/keymap.json &&
	rm -rf ~/.config/nvim/ &&
	cp -rf  ~/projects/configs/nvim                   ~/.config/ &&
	cp      ~/projects/configs/emacs/init.el          ~/.emacs.d/init.el &&
	cp      ~/projects/configs/emacs/emacs.org        ~/eSync/org/emacs.org

    echo "done"
}

read -p "Do you want to proceed restore, this will reset all your local files with the files from configs? (y/n) " choice

case "$choice" in
    y|Y ) echo "Proceeding to the restore action."
	  restore_action
	  ;;
    n|N ) echo "Exiting the restore script." && exit;;
    * ) echo "Invalid choice. Exiting the restore script." && exit;;
esac

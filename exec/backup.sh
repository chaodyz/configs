#!/usr/bin/env bash

function backup_action() {
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
	
	cp ~/.config/alacritty/alacritty.toml  ~/projects/configs/alacritty.toml &&

	cp ~/.config/joplin/keymap.json  ~/projects/configs/keymap.json &&

	cp -rf ~/.config/nvim  ~/projects/configs/ &&
	
	cp ~/.emacs.d/init.el ~/projects/configs/emacs/init.el &&

	# backup org files
	cp ~/eSync/org/emacs.org ~/projects/configs/emacs/emacs.org

    echo "done"
}

read -p "Do you want to proceed backup, this will reset your projects/config? (y/n) " choice

case "$choice" in
    y|Y ) echo "Proceeding to the backup action."
	  backup_action
	  ;;
    n|N ) echo "Exiting the backup script." && exit;;
    * ) echo "Invalid choice. Exiting the backup script." && exit;;
esac





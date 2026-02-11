# ~/.bashrc 
### EXPORT
export TERM_PROGRAM=emacs
export TERM="xterm-256color"                      # getting proper colors
export HISTCONTROL=ignoredups:erasedups           # no duplicate entries
# export TERMINFO=/usr/share/terminfo
### SET VI MODE ###
set -o vi
bind -m vi-command 'Control-l: clear-screen'
bind -m vi-insert 'Control-l: clear-screen'
bind '"\e[A": history-search-backward'
bind '"\e[B": history-search-forward'

# vim and nvim
alias vim="nvim"
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
### AUTO COMPLETE NAME complete -cf sudo

### SHOPT
shopt -s autocd # change to named directory
shopt -s cdspell # autocorrects cd misspellings
shopt -s cmdhist # save multi-line commands in history as single line
shopt -s dotglob
shopt -s histappend # do not overwrite history shopt -s expand_aliases # expand aliases
shopt -s checkwinsize # checks term size when bash regains control
#ignore upper and lowercase when TAB completion
bind "set completion-ignore-case on"

# Colorize grep output (good for log files)
alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'

# confirm before overwriting something
alias cp="cp -i"
alias mv='mv -i'
alias rm='rm -i'
# adding flags alias df='df -h'                          # human-readable sizes system shortcut alias ls='ls -G'

# Git
alias gc="git checkout"

if [[ "$OSTYPE" == "linux-gnu" ]]; then
    #----Linux ----------------------
    #  (\(\  
    #  (-.-)
    # o_(")(")
    # 1. Check if the operating system is Linux

    #echo "Running on Linux"
    echo "            ."
    echo "         .   :   ."
    echo "     '.   .  :  .   .'"
    echo "  ._   '._.-'''-._.'   _."
    echo "    '-..'         '..-' "
    echo " --._ /.==.     .==.\ _.--"
    echo "     ;/_o__\\   /_o__\\;"
    echo "-----|'     ) (     '|-----"
    echo "    _: \\_) (\\_/) (_/ ;_"
    echo " --'  \\  '._.=._.'  /  '--"
    echo "   _.-''.  '._.'  .''-._"
    echo "  '    .''.(_).''.    '"
    echo " .'   '  :  '   '."
    echo "        '    :   '"
    echo "             '"

    alias ll='ls -la --color=auto'
    alias l.='ls -d .[^.]* --color=auto'
    
    # shortcut
    alias joplin='~/.joplin/Joplin.AppImage'
    alias wifi='nmtui'
    alias vpn='wg-quick up se-mma-wg-001' # Call wireguard config to connect to Malmo, Sweden
    # 'bluetoothctl'
    # sound ctl 'alsamixer'
    export PATH="/opt/cmake/3.27.4/bin:$PATH"
    #CP
    alias cp-activate="source .nodeenv/bin/activate"
    alias cp-start="pm2 start ecosystem.dev.config.js"
    alias cp-restart="pm2 restart --update-env ecosystem.dev.config.js"
    alias cp-status="pm2 start ecosystem.dev.config.js"
    alias cp-logs="pm2 logs"
    alias cp-stop="pm2 stop ecosystem.dev.config.js"

    alias kde-restart="kquitapp5 plasmashell && kstart5 plasmashell"


    # Development
    # Terminal prompt
    eval "$(starship init bash)"
    # fast node manager
     eval "$(fnm env --use-on-cd --shell bash)"

    # Java
	[ -s "/Users/$USER/.jabba/jabba.sh" ] && source "/Users/$USER/.jabba/jabba.sh"

    #   .--.
    #  |o_o |
    #  |:_/ |
    # //   \ \
    # (|     | )
    # /'\_   _/`\
    # \___)=(___/
    # -------------------------------
fi

if [[ "$OSTYPE" == "darwin"* ]]; then
    # -------------------------------
    #  /\_/\  
    # ( o.o ) 
    #  > ^ <
    # Mac
    # Check if the operating system is Linux
    #echo "Running on MacOS"
    alias ll='ls -la -G'
    alias l.='ls -d .* -G'
    
    # MacOS common
    export PATH=/opt/homebrew/bin:$PATH
    export PATH=/opt/homebrew/sbin:$PATH
    export PATH="~/.local/bin:$PATH"
    export PATH="/data/data/com.termux/files/usr/bin:$PATH"

    # assume i have installed 'locate' for macos
    alias updatedb='sudo /usr/libexec/locate.updatedb'
    # Remove with status bar
    alias rmprog='function __rmprog() { rm -r "$@" | pv -lep -s $(find "$@" | wc -l) >/dev/null; }; __rmprog'
   
    # Personal
    # if [[ $USER == "diz" ]]; then
	# echo "你好，祝你好运！"
	# echo "    *  / \\_ *  / \\_      _  *        *   /'__        *"
	# echo "      /    \\  /    \\,   ((        .    _/  /  \\  *'."
	# echo " .   /\\/\\  /\\/ :' __ \\_  \`          _^/  ^/    \`--."
	# echo "    /    \\/  \\  _/  \\-'\      *    /.' ^_   \\_   .\\'  *"
	# echo "  /\\  .-   \`. \\/     \\ /==~=-=~=-=-;.  _/ \\ -. \`_/   \\"
	# echo " /  \`-.__ ^   / .-'.--\\ =-=~_=-=~=^/  _ \`--./ .-'  \`-"
	# echo "/        \`.  / /       \`.~-^=-=~=^=.-'      '-._ \`._"
	#    else
	# Work Machine
	# echo "Bienvenue Di, bon journey!"
	# echo "              |    |    |               "
	# echo "             )_)  )_)  )_)              "
	# echo "            )___))___))___)\            "
	# echo "           )____)____)_____)\\          "
	# echo "         _____|____|____|____\\\__      "
	# echo "---------\\                   /---------"
	# echo "  ^^^^^ ^^^^^^^^^^^^^^^^^^^^^          "
	# echo "    ^^^^      ^^^^     ^^^    ^^        "
	# echo "         ^^^^      ^^^^                "
    # K8s
	alias k='kubectl'

	export NODE_OPTIONS=--max_old_space_size=8192

    git-personal() {
      eval "$(ssh-agent -s)"
      ssh-add ~/.ssh/id_ed25519_personal
      ssh -T git@github.com
    }

    git-work() {
      eval "$(ssh-agent -s)"
      ssh-add ~/.ssh/id_ed25519_work
      ssh -T git@github.com
    }

    # GPG
    export GPG_TTY=$(tty)

    # Rust
	export PATH=$HOME/.cargo/env:$PATH
	# Version managers
    # Python
    alias brew='env PATH="${PATH//$(pyenv root)\/shims:/}" brew'
    # Node
    eval "$(fnm env --use-on-cd --shell bash)"
    # Java
	[ -s "/Users/$USER/.jabba/jabba.sh" ] && source "/Users/$USER/.jabba/jabba.sh"

	# The next line updates PATH for the Google Cloud SDK.
	if [ -f '/Users/$USER/dev/google-cloud-sdk/path.bash.inc' ]; then . '/Users/$USER/dev/google-cloud-sdk/path.bash.inc'; fi

	# The next line enables shell command completion for gcloud.
	if [ -f '/Users/$USER/dev/google-cloud-sdk/completion.bash.inc' ]; then . '/Users/$USER/dev/google-cloud-sdk/completion.bash.inc'; fi

    # GNU libtool
    PATH="/opt/homebrew/opt/libtool/libexec/gnubin:$PATH"

    # Terminal Prompt
    eval "$(starship init bash)"
    # fi
    #end of macos 
    #  c(._.)o
    #   /)_")
    #    / \
    #------------------------------------------
fi




export PATH="$HOME/.local/bin:$PATH"

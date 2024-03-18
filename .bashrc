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
shopt -s histappend # do not overwrite history
shopt -s expand_aliases # expand aliases
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

# adding flags
alias df='df -h'                          # human-readable sizes
# system shortcut
alias ls='ls -G'
## Use a long listing format ##
alias ll='ls -la -G'
## Show hidden files ##
alias l.='ls -d .* -G'

# Git
alias gc="git checkout"

if [[ "$OSTYPE" == "linux-gnu" ]]; then
    #----Archlinux ----------------------
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
    
    # shortcut
    alias joplin='~/.joplin/Joplin.AppImage'
    alias wifi='nmtui'
    alias vpn='wg-quick up se-mma-wg-001' # Call wireguard config to connect to Malmo, Sweden
    # 'bluetoothctl'
    # sound ctl 'alsamixer'
    export PATH="/opt/cmake/3.27.4/bin:$PATH"

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
    if [[ $USER == "diz" ]]; then
	export PATH=$HOME/.cargo/env:$PATH
	echo "你好，祝你好运！"
	echo "    *  / \\_ *  / \\_      _  *        *   /'__        *"
	echo "      /    \\  /    \\,   ((        .    _/  /  \\  *'."
	echo " .   /\\/\\  /\\/ :' __ \\_  \`          _^/  ^/    \`--."
	echo "    /    \\/  \\  _/  \\-'\      *    /.' ^_   \\_   .\\'  *"
	echo "  /\\  .-   \`. \\/     \\ /==~=-=~=-=-;.  _/ \\ -. \`_/   \\"
	echo " /  \`-.__ ^   / .-'.--\\ =-=~_=-=~=^/  _ \`--./ .-'  \`-"
	echo "/        \`.  / /       \`.~-^=-=~=^=.-'      '-._ \`._"
    else
	# Work Machine
	echo "Bienvenue Di, bon journey!"
	echo "              |    |    |               "
	echo "             )_)  )_)  )_)              "
	echo "            )___))___))___)\            "
	echo "           )____)____)_____)\\          "
	echo "         _____|____|____|____\\\__      "
	echo "---------\\                   /---------"
	echo "  ^^^^^ ^^^^^^^^^^^^^^^^^^^^^          "
	echo "    ^^^^      ^^^^     ^^^    ^^        "
	echo "         ^^^^      ^^^^                "
	alias k='kubectl'
	# Life work crafting team
	alias wc-run='cd /Users/$USER/Projects/ultipro-app&&npm run start:engaging-developing-web'
	alias wc-test='cd /Users/$USER/Projects/ultipro-app&&npm run test:engaging-developing-web:watch'
	alias wc-cypress='cd /Users/$USER/Projects/ultipro-app&&npm run test:engaging-developing-web:cypress'
	alias wc-local='cd /Users/$USER/Projects/ed-localization&&npm start'
	alias mock-tms="cd /Users/$USER/Projects/mock-tms&&nodemon"
	alias wc-format="cd /Users/$USER/Projects/ultipro-app&&nx format:write&&nx affected:lint"

	alias one-serve="cd /Users/$USER/projects/mobile-ultipro-app&&rm -rf www dist temp&&npm run one-app:plugin:serve"
	
	alias one-pkginstall="cd /Users/$USER/projects/mobile-ultipro-app&&rm -rf www dist temp&&npm run one-app:plugin:package&&npm run one-app:plugin:install"

	alias ukgpro="npm run one-app:ukgpro:livereload:android"
	alias authflow="npm run one-app:authflow:livereload:android"
	alias handler="npm run one-app:handler:livereload:android"
	export NODE_OPTIONS=--max_old_space_size=8192
	# python version manager
	export PATH="$(pyenv root)/shims:${PATH}"

	export M2_HOME=/opt/apache-maven-3.8.4
	# export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.8.0_321.jdk/Contents/Home
	# export JAVA_HOME=/Library/Java/JavaVirtualMachines/zulu-8.jdk/Contents/Home
	export JAVA_HOME=/Library/Java/JavaVirtualMachines/zulu-11.jdk/Contents/Home/
	export ANDROID_SDK_ROOT=/Users/$USER/Library/Android/sdk
	export ANDROID_HOME=/Users/$USER/Library/Android/sdk
	export PATH=$ANDROID_HOME/emulator:$ANDROID_HOME/tools:$PATH
	export GRADLE_HOME=/opt/gradle-7.1.1
	export PATH=$GRADLE_HOME/bin:$PATH
	export PATH=$JAVA_HOME/bin:$M2_HOME/bin:$PATH 
	export PATH="/opt/homebrew/opt/ruby/bin:$PATH"

	eval "$(fnm env --use-on-cd)"
	[ -s "/Users/$USER/.jabba/jabba.sh" ] && source "/Users/$USER/.jabba/jabba.sh"
	# The next line updates PATH for the Google Cloud SDK.
	if [ -f '/Users/$USER/dev/google-cloud-sdk/path.bash.inc' ]; then . '/Users/$USER/dev/google-cloud-sdk/path.bash.inc'; fi

	# The next line enables shell command completion for gcloud.
	if [ -f '/Users/$USER/dev/google-cloud-sdk/completion.bash.inc' ]; then . '/Users/$USER/dev/google-cloud-sdk/completion.bash.inc'; fi
    fi #end of macos 
    #  c(._.)o
    #   /)_")
    #    / \
    #------------------------------------------
fi

 eval "$(starship init bash)"




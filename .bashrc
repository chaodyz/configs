# ~/.bashrc If not running interactively, don't do anything [[ $- != *i* ]] && return comment out for starship PS1='[\u@\h \W]\$ '
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

### AUTO COMPLETE NAME
complete -cf sudo

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

# alias updatedb = 'sudo /usr/libexec/locate.updatedb'
alias updatedb='sudo /usr/libexec/locate.updatedb'
# vim
alias vim="nvim"

alias k='kubectl'
alias wc-run='cd /Users/$USER/Projects/ultipro-app&&npm run start:engaging-developing-web'
alias wc-test='cd /Users/$USER/Projects/ultipro-app&&npm run test:engaging-developing-web:watch'
alias wc-cypress='cd /Users/$USER/Projects/ultipro-app&&npm run test:engaging-developing-web:cypress'
alias wc-local='cd /Users/$USER/Projects/ed-localization&&npm start'
alias mock-tms="cd /Users/$USER/Projects/mock-tms&&nodemon"
alias wc-format="cd /Users/$USER/Projects/ultipro-app&&nx format:write&&nx affected:lint"

alias gc="git checkout"

# emacs 
export PATH="/Users/$USER/.emacs.d/bin/doom:$PATH"
export PATH="/opt/homebrew/Cellar/emacs-mac/emacs-28.2-mac-9.1/bin/:$PATH"

export PATH=/opt/homebrew/bin:$PATH
export PATH=/opt/homebrew/sbin:$PATH

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
# python version manager
export PATH="$(pyenv root)/shims:${PATH}"

export M2_HOME=/opt/apache-maven-3.8.4
export NODE_OPTIONS=--max_old_space_size=8192
# export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.8.0_321.jdk/Contents/Home
JAVA_HOME=/Library/Java/JavaVirtualMachines/zulu-11.jdk/Contents/Home/
export ANDROID_SDK_ROOT=/Users/$USER/Library/Android/sdk
export ANDROID_HOME=/Users/$USER/Library/Android/sdk
export GRADLE_HOME=/usr/local/opt/gradle@7
export PATH=$ANDROID_HOME/emulator:$ANDROID_HOME/tools:$PATH
export PATH=$GRADLE_HOME/bin:$PATH
export PATH="~/.local/bin:$JAVA_HOME/bin:$M2_HOME/bin:$PATH"
. "$HOME/.cargo/env"

eval "$(starship init bash)"
eval "$(fnm env --use-on-cd)"

[ -s "/Users/$USER/.jabba/jabba.sh" ] && source "/Users/$USER/.jabba/jabba.sh"
# export JAVA_HOME=/Library/Java/JavaVirtualMachines/zulu-8.jdk/Contents/Home

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/$USER/dev/google-cloud-sdk/path.bash.inc' ]; then . '/Users/$USER/dev/google-cloud-sdk/path.bash.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/$USER/dev/google-cloud-sdk/completion.bash.inc' ]; then . '/Users/$USER/dev/google-cloud-sdk/completion.bash.inc'; fi

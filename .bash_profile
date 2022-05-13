# ~/.bashrc If not running interactively, don't do anything [[ $- != *i* ]] && return comment out for starship PS1='[\u@\h \W]\$ '
### EXPORT
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
alias wc-run='cd /Users/diz/Projects/ultipro-app&&npm run start:engaging-developing-web'
alias wc-test='cd /Users/diz/Projects/ultipro-app&&npm run test:engaging-developing-web:watch'
alias wc-cypress='cd /Users/diz/Projects/ultipro-app&&npm run test:engaging-developing-web:cypress'
alias wc-local='cd /Users/diz/Projects/ed-localization&&npm start'
alias mock-tms="cd /Users/diz/Projects/mock-tms&&nodemon"
alias wc-format="cd /Users/diz/Projects/ultipro-app&&nx format:write&&nx affected:lint"

alias gc="git checkout"
alias gp="git pull"
alias gpp="git push"
alias gf="git fetch"
alias gcc="git commit"

M2_HOME=/opt/apache-maven-3.8.4
JAVA_HOME=/Library/Java/JavaVirtualMachines/zulu-11.jdk/Contents/Home/
export PATH="~/.local/bin:$JAVA_HOME/bin:$M2_HOME/bin:$PATH"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

. "$HOME/.cargo/env"


# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/diz/google-cloud-sdk/path.bash.inc' ]; then . '/Users/diz/google-cloud-sdk/path.bash.inc'; fi

# The next line enables shell command completion for gcloud.

if [ -f '/Users/diz/google-cloud-sdk/completion.bash.inc' ]; then . '/Users/diz/google-cloud-sdk/completion.bash.inc'; fi


eval "$(starship init bash)"

[ -s "/Users/diz/.jabba/jabba.sh" ] && source "/Users/diz/.jabba/jabba.sh"

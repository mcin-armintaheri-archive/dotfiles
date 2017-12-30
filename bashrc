#
# /etc/bash.bashrc
#

alias ls='ls --color'

export TERM=xterm-256color
# Git branch in prompt.

parse_git_branch() {
    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}

edf() {
    $HOME/local/bin/emacs $* 2> /dev/null &
    disown
}

ihsk() {
    docker run -it --volume $(pwd):/ihaskell_notebooks --publish 8888:8888 gibiansky/ihaskell:latest
}

txtblk='\e[0;30m' # Black - Regular
txtred='\e[0;31m' # Red
txtgrn='\e[0;32m' # Green
txtylw='\e[0;33m' # Yellow
txtblu='\e[0;34m' # Blue
txtpur='\e[0;35m' # Purple
txtcyn='\e[0;36m' # Cyan
txtwht='\e[0;37m' # White
bldblk='\e[1;30m' # Black - Bold
bldred='\e[1;31m' # Red
bldgrn='\e[1;32m' # Green
bldylw='\e[1;33m' # Yellow
bldblu='\e[1;34m' # Blue
bldpur='\e[1;35m' # Purple
bldcyn='\e[1;36m' # Cyan
bldwht='\e[1;37m' # White
unkblk='\e[4;30m' # Black - Underline
undred='\e[4;31m' # Red
undgrn='\e[4;32m' # Green
undylw='\e[4;33m' # Yellow
undblu='\e[4;34m' # Blue
undpur='\e[4;35m' # Purple
undcyn='\e[4;36m' # Cyan
undwht='\e[4;37m' # White
bakblk='\e[40m'   # Black - Background
bakred='\e[41m'   # Red
bakgrn='\e[42m'   # Green
bakylw='\e[43m'   # Yellow
bakblu='\e[44m'   # Blue
bakpur='\e[45m'   # Purple
bakcyn='\e[46m'   # Cyan
bakwht='\e[47m'   # White
txtrst='\e[0m'    # Text Reset

# If not running interactively, don't do anything
[[ $- != *i* ]] && return


#PS1='[\u@\h \W]\$ '
#TLL=$(tput cup "$LINES")
PS1="\[$txtgrn\][\u]"
if [[ SESSION_TYPE == "remote/ssh" ]]; then
    PS1="$PS1\[$txtred\][\h]"
else
    PS1="$PS1\[$txtpur\][\h]"
fi
PS1="$PS1\[$txtylw\][\W]"
PS1="$PS1\[$txtcyn\] ::\$(parse_git_branch)"
PS1="$PS1\n\[$txtcyn\]\$\[$txtrst\] "
PS2='> '
PS3='> '
PS4='+ '

case ${TERM} in
  xterm*|rxvt*|Eterm|aterm|kterm|gnome*)
    PROMPT_COMMAND=${PROMPT_COMMAND:+$PROMPT_COMMAND; }'printf "\033]0;%s@%s:%s\007" "${USER}" "${HOSTNAME%%.*}" "${PWD/#$HOME/\~}"'

    ;;
  screen)
    PROMPT_COMMAND=${PROMPT_COMMAND:+$PROMPT_COMMAND; }'printf "\033_%s@%s:%s\033\\" "${USER}" "${HOSTNAME%%.*}" "${PWD/#$HOME/\~}"'
    ;;
esac

[ -r /usr/share/bash-completion/bash_completion   ] && . /usr/share/bash-completion/bash_completion
export PATH="$HOME/local/bin:$HOME/.local/bin:$PATH"

# export node path
export PATH="$HOME/node/bin:$PATH"

export NODE_PATH="$HOME/local/npm-global/lib/node_modules"


# added by Anaconda3 2.5.0 installer
export PATH="$HOME/local/Anaconda/bin:$PATH"

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"

# Haskell path
export PATH="$HOME/.stack/programs/x86_64-linux/ghc-8.0.2/bin:/opt/cabal/1.22/bin:/opt/ghc/7.10.3/bin:$HOME/.cabal/bin::$PATH"

# Filewell tools
export PATH="$PATH:~/plotly/streambed/shelly/filewell/bin/"

# Go path
export GOROOT="$HOME/local/go"
export PATH="$PATH:$HOME/local/go/bin"
export GOPATH="$HOME/.go_modules/"
export PATH="$PATH:$GOPATH/bin"

# asdf version manager
if [[ -e $HOME/repos/asdf ]]; then
  . $HOME/repos/asdf/asdf.sh
  . $HOME/repos/asdf/completions/asdf.bash
fi

export BOOT_CLOJURE_VERSION="1.8.0"

if [[ -e $HOME/local/startup && -n `ls $HOME/local/startup/` ]]; then
    for i in $HOME/local/startup/*; do 
        source $i;
    done
fi

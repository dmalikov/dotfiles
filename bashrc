if [[ $- != *i* ]] ; then
    # Shell is non-interactive.  Be done now!
    return
fi

# Put your fun stuff here.
source /etc/profile

# vi-like interface
set -o vi

# autocd
shopt -s autocd

# ^l clear screen
bind -m vi-insert "\C-l":clear-screen

export TERM='rxvt-256color'
export COLORTERM='rxvt-unicode-256color'
export EDITOR=vim

PATH=${HOME}/bin/:$PATH:${HOME}/.cabal/bin:${HOME}/.smackage/bin:
export PATH

HISTCONTROL=ignoredups:ignorespace
PROMPT_COMMAND=''

export PS1="\[\033[0;33m\][ \[\033[0;32m\]\w\[\033[0;33m\] ]\[\033[0;33m\] \n\[\033[0m\]$> "


# external monitor helper
declare MAINDISPLAY=LVDS1
setdisplay() {
    local -r NEWDISPLAY=${2:-DP1}
    source ${HOME}/prexinit
    case $1 in
            on)     xrandr --output $NEWDISPLAY --same-as $MAINDISPLAY --auto ;;
            off)    xrandr --output $NEWDISPLAY --off ;;
            *)      echo "Wrong argument"; return 1 ;;
    esac
}

alias xinit_='cp ~/.xinit_log{,_old} && xinit |& tee ~/.xinit_log'
alias sml='rlwrap sml'
alias ssh='TERM=xterm-color ssh'
alias racket='rlwrap racket'
alias mvn='mvn-2.2'
alias git-home='cd $(git rev-parse --show-toplevel)'
alias ihach='iconv -cf inis-cyrillic'

# xmobar stuff
setxmobar () {
    ghc -O2 $HOME/dmalikov/dotfiles/xmobar.hs -o ~/.xmonad/xmobar -fforce-recomp
}

setxmobar-top () {
    ghc -O2 $HOME/dmalikov/dotfiles/xmobar-top.hs -o ~/.xmonad/xmobar-top -fforce-recomp
}

setxmobar-wide () {
    ghc -O2 $HOME/dmalikov/dotfiles/xmobar-wide.hs -o ~/.xmonad/xmobar -fforce-recomp
}

# knife stuff
source ~/.bash-qik-stuff
function knife-opscode () {
    knife $@ -c ~/.chef/knife-opscode.rb
}

# ruby stuff
unset RUBYOPT
PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting

# /etc/skel/.bashrc
#
# This file is sourced by all *interactive* bash shells on startup,
# including some apparently interactive shells such as scp and rcp
# that can't tolerate any output.  So make sure this doesn't display
# anything or bad things will happen !


# Test for an interactive shell.  There is no need to set anything
# past this point for scp and rcp, and it's important to refrain from
# outputting anything in those cases.
if [[ $- != *i* ]] ; then
    # Shell is non-interactive.  Be done now!
    return
fi

# Put your fun stuff here.
source /etc/profile

# vi-like interface
set -o vi

# ^l clear screen
bind -m vi-insert "\C-l":clear-screen

export TERM='rxvt-256color'
export COLORTERM='rxvt-unicode-256color'
export EDITOR=vim

PATH=$PATH:${HOME}/bin:${HOME}/.cabal/bin
export PATH

HISTCONTROL=ignoredups:ignorespace
PROMPT_COMMAND=''

export PS1="\[\033[0;33m\][ \[\033[0;32m\]\w\[\033[0;33m\] ]\[\033[0;33m\] \n\[\033[0m\]$> "

alias mvn='mvn-2.2'
alias mc='mc --colors normal=green,default:selected=brightmagenta,gray:marked=yellow,default:markselect=yellow,gray:directory=blue,default:executable=brightgreen,default:link=cyan,default:device=brightmagenta,default:special=lightgray,default:errors=red,default:reverse=green,default:gauge=green,default:input=white,gray:dnormal=green,gray:dfocus=brightgreen,gray:dhotnormal=cyan,gray:dhotfocus=brightcyan,gray:menu=green,default:menuhot=cyan,default:menusel=green,gray:menuhotsel=cyan,default:helpnormal=cyan,default:editnormal=green,default:editbold=blue,default:editmarked=gray,blue:stalelink=red,default'

function remove-spaces () {
  filename=$1
  if [[ -z "$filename" ]]; then
    echo "filename undefined"
    break
  else
    newfilename=`echo "${filename}" | sed -r 's/\s/_/g'`
    mv "${filename}" "${newfilename}" -v
  fi
}

declare MAINDISPLAY=LVDS-0

setdisplay() {
    local -r NEWDISPLAY=${2:-DP-0}
    case $1 in
            on)     xrandr --output $NEWDISPLAY --same-as $MAINDISPLAY --auto ;;
            off)    xrandr --output $NEWDISPLAY --off ;;
            *)      echo "Wrong argument"; return 1 ;;
    esac
}

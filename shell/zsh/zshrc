autoload -U compinit promptinit colors

# completion settings
compinit
zstyle ':completion::complete:*' use-cache 1

# prompt settings
promptinit
colors
precmd() { print -rP ""; print -rP "$fg[yellow][ $fg[green]%~ $fg[yellow]]" }
export PROMPT="$> "

# number of lines kept in history
export HISTSIZE=1000
# number of lines saved in the history after logout
export SAVEHIST=1000
# location of history
export HISTFILE=~/.zhistory
# append command to history file once executed
setopt inc_append_history
# autocompletion of command line switches for aliases
setopt completealiases

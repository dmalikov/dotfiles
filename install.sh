#!/bin/bash

ln -sf --verbose "$(pwd)/vimrc" ~/.vimrc
ln -sf --verbose "$(pwd)/apvlvrc" ~/.apvlvrc
ln -sf --verbose "$(pwd)/conkyrc" ~/.conkyrc
ln -sf --verbose "$(pwd)/screenrc" ~/.screenrc
ln -sf --verbose "$(pwd)/gvimrc" ~/.gvimrc
ln -sf --verbose "$(pwd)/xinitrc" ~/.xinitrc
ln -sf --verbose "$(pwd)/Xdefaults" ~/.Xdefaults
ln -sf --verbose "$(pwd)/pentadactylrc" ~/.pentadactylrc
ln -sf --verbose "$(pwd)/gitconfig" ~/.gitconfig
ln -sf --verbose "$(pwd)/ghci" ~/.ghci

echo "done!"


#!/bin/bash

ln -sf --verbose "$(pwd)/.vimrc"     ~/.vimrc
ln -sf --verbose "$(pwd)/.apvlvrc"   ~/.apvlvrc
ln -sf --verbose "$(pwd)/.conkyrc"   ~/.conkyrc
ln -sf --verbose "$(pwd)/.screenrc"  ~/.screenrc
ln -sf --verbose "$(pwd)/.gvimrc"    ~/.gvimrc

echo "done!"


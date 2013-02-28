#! /bin/bash

# bash
cp bashrc ~/.bashrc

# vim
VIM_CFG_DIR="$HOME/.vim"

cp vimrc ~/.vimrc
if [ ! -d $VIM_CFG_DIR ]; then
    mkdir $VIM_CFG_DIR
fi
cp vim/* "$VIM_CFG_DIR/" -rf

# gdbinit
cp gdbinit ~/.gdbinit

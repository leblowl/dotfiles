#!/usr/bin/env bash

install_dir=~/.emacs.d/lisp
mkdir -p $install_dir
ln -s $PWD/init.el $install_dir/init.el
ln -s $PWD/keys.el $install_dir/keys.el
ln -s $PWD/ob-racket.el $install_dir/ob-racket.el

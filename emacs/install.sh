#!/usr/bin/env bash

base_dir=~/.emacs.d
install_dir=$base_dir/lisp
mkdir -p $install_dir
ln -s $PWD/init.el $base_dir/init.el
ln -s $PWD/keys.el $install_dir/keys.el
ln -s $PWD/ob-racket.el $install_dir/ob-racket.el
ln -s $PWD/yaml-mode.el $install_dir/yaml-mode.el

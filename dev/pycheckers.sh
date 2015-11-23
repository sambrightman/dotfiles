#!/usr/bin/env bash

# sudo pip install flake8 pylint pylint-venv
# configuration for both is in dotfiles repo

flake8 "$1"
python ~/dev/emacs-for-python/scripts/pylint-mod.py "$1"
true

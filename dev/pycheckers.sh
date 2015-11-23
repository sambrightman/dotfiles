#!/usr/bin/env bash --login

workon emacs-for-python
flake8 --ignore=E501 --max-complexity=10 "$1"
python ~/dev/emacs-for-python/scripts/pylint-mod.py "$1"
true

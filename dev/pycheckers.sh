#!/bin/bash

flake8 --ignore=E501 --max-complexity=10 "$1"
~/dev/emacs-for-python/scripts/pylint-mod.py "$1"
true

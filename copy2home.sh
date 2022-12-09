#!/usr/bin/env bash

BASE=$(dirname $0)

if [ -f "$HOME/.emacs" ]
then 
    cp "$HOME/.emacs" "$HOME/.emacs_$(date +%Y%m%d_%H%M%S_%3N).bak"
    cp "$BASE/.emacs" "$HOME/.emacs"
    cp -r "$BASE/.emacs.d" "$HOME"
fi

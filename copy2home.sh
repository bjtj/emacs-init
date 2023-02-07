#!/usr/bin/env bash

BASE=$(dirname $0)

if [ -f "$HOME/.emacs" ]
then 
    mv "$HOME/.emacs" "$HOME/.emacs_$(date +%Y%m%d_%H%M%S_%3N).bak"
fi

cp "$BASE/.emacs" "$HOME/.emacs"
cp -r "$BASE/.emacs.d" "$HOME"

#!/usr/bin/env zsh

f="$1"
args=$*
if [ -z "$1" ]; then
    f="."
    args="$f"
fi

if [ ! -e "$f" ]; then
  echo "bot_or_exa: $f does not exist."
else
  if [ -d "$f" ]; then
    exa --color=always -l --header $args
  else
    bat --color=always --line-range :1000 $args
  fi
fi

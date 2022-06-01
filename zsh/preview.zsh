#!/usr/bin/env zsh

f="$1"
args=$*
if [ -z "$1" ]; then
    f="."
    args="$f"
fi

mime_encooding=$(file --mime-encoding ${f} | awk '{print $(NF)}')
# echo "MIME ENCODING: ${mime_encoding}"

if [ ! -e "$f" ]; then
  echo "preview.zsh: $f does not exist."
else
  if [ -d "$f" ]; then
    exa --color=always -l --header $args
  elif [ "${mime_encooding}" = "binary" ]; then
    hexyl --color=always $args
  else
    bat --color=always --line-range :1000 $args
  fi
fi

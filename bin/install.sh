#!/bin/sh

if [ ! -d ~/dotfiles ]; then
   echo "~/dotfiles does not exist.  aborting."
   exit
fi

RM=/bin/rm
LN=/bin/ln

${RM} -f ~/.bashrc
${RM} -f ~/.profile

${LN} -s ~/dotfiles/bashrc ~/.bashrc
${LN} -s ~/dotfiles/profile ~/.profile
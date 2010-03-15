#!/bin/sh

if [ ! -d ~/dotfiles ]; then
   echo "~/dotfiles does not exist.  aborting."
   exit
fi

RM=/bin/rm
LN=/bin/ln

${RM} ~/.bashrc
${RM} ~/.profile

${LN} -s ~/.bashrc ~/dotfiles/bashrc
${LN} -s ~/.profile ~/dotfiles/profile
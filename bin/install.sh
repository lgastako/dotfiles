#!/bin/bash

if [ ! -d ~/dotfiles ]; then
   echo "~/dotfiles does not exist.  aborting."
   exit
fi

RM=/bin/rm
LN=/bin/ln

${RM} -f ~/.bashrc
${RM} -f ~/.profile
${RM} -f ~/.gitconfig
${RM} -f ~/.gitignore

${LN} -s ~/dotfiles/bashrc ~/.bashrc
${LN} -s ~/dotfiles/profile ~/.profile
${LN} -s ~/dotfiles/gitconfig ~/.gitconfig

mkdir -p ~/dotfiles/tags

for tag in $*
do
   if [ "$tag" == "README" ]
   then
     continue
   fi
   src=~/dotfiles/tags/available/$tag
   dst=~/dotfiles/tags/enabled/$tag
   if [ -e "$src" ]
   then
       if [ -e "$dst" ]
       then
           echo "Tag already exists: $tag"
       else
           ln -s $src $dst
       fi
   else
       echo "WARNING: Could not install tag: $tag"
   fi
done
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
${RM} -f ~/.inputrc
${RM} -f ~/.zshrc
${RM} -f ~/.oh-my-zsh

${LN} -s ~/dotfiles/bashrc ~/.bashrc
${LN} -s ~/dotfiles/profile ~/.profile
${LN} -s ~/dotfiles/gitconfig ~/.gitconfig
${LN} -s ~/dotfiles/inputrc ~/.inputrc
${LN} -s ~/dotfiles/zshrc ~/.zshrc
${LN} -s ~/dotfiles/oh-my-zsh ~/.oh-my-zsh

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

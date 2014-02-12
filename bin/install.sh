#!/bin/bash

if [ ! -d ~/dotfiles ]; then
   echo "~/dotfiles does not exist.  aborting."
   exit
fi

mkdir -p ~/local/bin
mkdir -p ~/dotfiles/tags

RM=/bin/rm
LN=/bin/ln

${RM} -f ~/.bashrc
${RM} -f ~/.profile
${RM} -f ~/.gitconfig
${RM} -f ~/.gitignore
${RM} -f ~/.inputrc
${RM} -f ~/.zshrc
${RM} -f ~/.oh-my-zsh
${RM} -f ~/.pdbrc
${RM} -f ~/.xmonad
${RM} -rf ~/.gtkrc-2.0
${RM} -rf ~/.emacs.d
${RM} -f ~/.ssh/config
${RM} -f ~/.ackrc
${RM} -f ~/.vimrc
${RM} -rf ~/.vim
${RM} -rf ~/local/bin/mvim
${RM} -rf ~/.lein
${RM} -rf ~/.psqlrc

${LN} -s ~/dotfiles/bash/bashrc ~/.bashrc
${LN} -s ~/dotfiles/bash/profile ~/.profile
${LN} -s ~/dotfiles/git/gitconfig ~/.gitconfig
${LN} -s ~/dotfiles/inputrc ~/.inputrc
${LN} -s ~/dotfiles/zsh/zshrc ~/.zshrc
${LN} -s ~/dotfiles/zsh/oh-my-zsh ~/.oh-my-zsh
${LN} -s ~/dotfiles/python/pdb/pdbrc ~/.pdbrc
${LN} -s ~/dotfiles/gtk/gtkrc-2.0 ~/.gtkrc-2.0
${LN} -s ~/dotfiles/emacs/emacs.d ~/.emacs.d
mkdir -p ~/.ssh
chmod og-rwx ~/.ssh
${LN} -s ~/dotfiles/ssh/config ~/.ssh/config
${LN} -s ~/dotfiles/ack/ackrc ~/.ackrc
${LN} -s ~/dotfiles/vim/vimrc ~/.vimrc
${LN} -s ~/dotfiles/vim/dotvim ~/.vim
${LN} -s ~/dotfiles/contrib/gvim/mvim ~/local/bin
${LN} -s ~/dotfiles/lein/dot.lein ~/.lein
${LN} -s ~/dotfiles/postgresql/psqlrc ~/.psqlrc

# symlinking whole directory
${LN} -s ~/dotfiles/xmonad ~/.xmonad


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


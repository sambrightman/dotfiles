#!/usr/bin/env bash

# FIXME: several commands are missing cd ${DEV_DIR}. the aim is/was to add consistent progress/logging.

/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

export DEV_DIR=~/dev

mkdir -p ~/.local/bin ~/bin
export PATH=$PATH:~/bin:~/.local/bin

brew bundle install

ln -nfs ${DEV_DIR}/dotfiles/.gitconfig* ~/
ln -nfs ${DEV_DIR}/dotfiles/.bash* ~/
touch ~/.bash_tokens ~/.bash_work
ln -nfs ${DEV_DIR}/dotfiles/bin/* ~/bin
find ${DEV_DIR}/dotfiles -maxdepth 1 -type f -name .* -print0 | xargs -r0 -I{} ln -nfs {} ~/

export PATH=$PATH:$(brew --prefix python)/libexec/bin
pip install --break-system-packages virtualenvwrapper
workon

gh repo clone cask/cask
cd cask && gh repo fork --remote
cd cask && git co less-dickish
ln -nfs ${DEV_DIR}/cask ~/.cask
ln -nfs ${DEV_DIR}/dotfiles/.emacs* ~/
# find emacs version
cp -a ${DEV_DIR}/dotfiles/melpa-removed/* ~/.emacs.d/.cask/31.1/elpa/
cd ~/.emacs.d && cask install
gh repo clone emacs-mirror/emacs
cd emacs && git co emacs-31.1

gh repo clone seebi/dircolors-solarized
ln -nfs ${DEV_DIR}/dircolors-solarized/dircolors.ansi-universal ~/.dircolors

gh repo clone lincheney/fzf-tab-completion
gh repo clone sambrightman/penvwrapper

curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.7/install.sh | PROFILE=/dev/null bash

curl -sSL https://get.rvm.io | bash -s stable
rvm rvmrc warning ignore ${DEV_DIR}/dotfiles/.rvmrc
rvm get master # stable not really being updated, even master not very well
rvm use ruby --install --default -E 'ac_cv_func_dup3=no,ac_cv_func_pipe2=no' # install advertised latest with workaround for https://bugs.ruby-lang.org/issues/22311
rvm @global do gem install travis
travis init

bash -c "sh <(curl -fsSL https://opam.ocaml.org/install.sh)"
opam init

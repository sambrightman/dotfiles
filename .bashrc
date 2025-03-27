export CODE_DIR="$HOME/code"
export DEV_DIR="$HOME/dev"

export HOMEBREW_TEMP=~/tmp
export HOMEBREW_NO_INSTALL_CLEANUP=
export HOMEBREW_NO_AUTO_UPDATE=
export HOMEBREW_NO_INSTALLED_DEPENDENTS_CHECK=
if [[ "$(hostname -s)" == "asgardstudy1" || "$(hostname -s)" == "asgardstudy2" ]]; then
    export HOMEBREW_FORCE_BREWED_CURL=1
fi

if [[ "${OSTYPE}" == "darwin"* ]]; then
    export HOMEBREW_PREFIX="/opt/homebrew";
elif [ -d "$HOME/.linuxbrew-centos" ]; then
    export HOMEBREW_PREFIX="$HOME/.linuxbrew-centos"
elif [ -d "$HOME/.linuxbrew" ]; then
    export HOMEBREW_PREFIX="$HOME/.linuxbrew"
else
    export HOMEBREW_PREFIX="/home/linuxbrew/.linuxbrew"
fi
eval "$(${HOMEBREW_PREFIX}/bin/brew shellenv)"

function require() {
    filename=${1:?usage: require filename [command=source]} && shift
    func=${1:-source}
    if [ -f "$filename" ]; then
        eval "$func $filename"
    else
        echo "require: $filename does not exist"
    fi
}

# workaround for silly completion
function nvm() {
    :
}

require "$(brew --prefix)/etc/bash_completion"

# for Solarized in emacs
unset COLORTERM
export TERM=xterm-16color
export TERMINFO="$(brew --prefix ncurses)/share/terminfo"
export SHELL=$BASH

# everything above comes before system /etc so it has correct inputs

if [ -f /etc/bashrc ]; then
	source /etc/bashrc
fi

function src() {
    source ~/.bash_profile
}

function proxy() {
    local proxy_url=$1 && shift
    local extra_no_proxies="$*"

    local keys=(http_proxy https_proxy all_proxy HTTP_PROXY HTTPS_PROXY ALL_PROXY no_proxy NO_PROXY)
    local key
    for key in ${keys[*]}; do
        if [ -z "${proxy_url}" ]; then
            echo "${key}=${!key}"
        elif [ "${proxy_url}" = "off" ]; then
            eval "unset ${key}"
        else
            if [ "${key,,}" == "no_proxy" ]; then
                eval "export ${key}=localhost,127.0.0.1$(printf ",%s" ${extra_no_proxies})"
            else
                eval "export ${key}=http://${proxy_url}"
            fi
        fi
    done
}

# manage prompt myself so that it works in new shells
export VIRTUAL_ENV_DISABLE_PROMPT=true
function virtual_env_prompt_prefix() {
    local virtual_env_name=$(basename "${VIRTUAL_ENV:-}")
    echo -n ${virtual_env_name:+($virtual_env_name)}
}

function evm_prompt_prefix() {
    local evm_config_path=~/.evm/.config
    if [ -f "${evm_config_path}" ]; then
        local evm_name=$(jq -M -r .current "${evm_config_path}")
        if [[ "${evm_name}" != "null" ]]; then
            echo -n ${evm_name:+($evm_name)}
        fi
    fi
}

function remove_path() {
    PATH=:$PATH:
    PATH=${PATH//":$1:"/:}
    PATH=${PATH%:}
    PATH=${PATH#:}
}

export PATH=~/bin:~/.local/bin:${PATH}

export GOPATH=${DEV_DIR}/go
export PATH=${PATH}:${GOPATH}/bin

export PATH="$(brew --prefix python)/libexec/bin:$PATH"
PATH=$HOME/.evm/bin:$PATH
PATH=$HOME/.cask/bin:$PATH
PATH=$HOME/.rvm/bin:$PATH

require ~/.fzf.bash
require ${DEV_DIR}/fzf-tab-completion/bash/fzf-bash-completion.sh
require ~/.travis/travis.sh

# https://github.com/moby/moby/issues/25450
# if [[ -n "$DOCKER_IMAGE_NAME" ]]; then
#     stty columns 237 rows 62
# fi

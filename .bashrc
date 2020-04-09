function require() {
    filename=${1:?usage: require filename [command=source]} && shift
    func=${1:-source}
    if [ -f "$filename" ]; then
        eval "$func $filename"
    else
        echo "require: $filename does not exist"
    fi
}

require "$(/usr/local/bin/brew --prefix)/etc/bash_completion"

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

PATH=$HOME/.evm/bin:$PATH
PATH=$HOME/.cask/bin:$PATH
PATH=$HOME/.rvm/bin:$PATH

require ~/.fzf.bash
require ~/.travis/travis.sh

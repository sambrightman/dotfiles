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
    local evm_name=$(jq -Mr .current ~/.evm/.config)
    if [[ "${evm_name}" != "null" ]]; then
        echo -n ${evm_name:+($evm_name)}
    fi
}

PATH=$HOME/.rvm/bin:$PATH
PATH=$HOME/.evm/bin:$PATH
require ~/.fzf.bash
require ~/.travis/travis.sh

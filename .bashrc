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
[ -f ~/.fzf.bash ] && source ~/.fzf.bash

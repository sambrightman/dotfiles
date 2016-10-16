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
    echo ${virtual_env_name:+($virtual_env_name)}
}

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
[ -f ~/.fzf.bash ] && source ~/.fzf.bash

source ~/.bashrc

export WORKON_HOME=~/.virtualenvs
mkdir -p $WORKON_HOME
require /usr/local/bin/virtualenvwrapper.sh
export PIP_VIRTUALENV_BASE=$WORKON_HOME
export PIP_RESPECT_VIRTUALENV=true
export PROJECT_HOME=$HOME/dev

# perl
export PWORKON_HOME=~/.pvirtualenvs

export PROMPT_COMMAND="history -a"
export HISTCONTROL=ignoreboth

function proxy() {
    local proxy_host=${1:-proxy}
    local proxy_port=3128

    # local gitproxy=~/bin/gitproxy

    local keys=( http_proxy https_proxy all_proxy HTTP_PROXY HTTPS_PROXY ALL_PROXY )
    local key
    for key in ${keys[*]}; do
	if [ $proxy_host = "off" ]; then
	    eval unset $key
	    # eval unset GIT_PROXY_COMMAND
	else
	    eval export $key=http://$proxy_host:$proxy_port/
	    # echo "echo \$*" > $gitproxy
	    # echo "nc -x$proxy_host:$proxy_port -X5 \$*" >> $gitproxy
	    # chmod +x $gitproxy
	    # eval export GIT_PROXY_COMMAND=$gitproxy
	fi
    done
}

# for Solarized in emacs
export TERM=xterm

export EMACSCLIENT='/usr/local/bin/emacsclient -q -a "" -t'
#export EMACSCLIENT='/usr/bin/emacs'
export EDITOR=${EMACSCLIENT}
function emacs() {
    eval ${EMACSCLIENT} $@
}
function stopemacs() {
    /usr/local/bin/emacsclient -q -e "(save-buffers-kill-emacs)" $@
}
function killemacs() {
    /usr/local/bin/emacsclient -q -e "(kill-emacs)" $@
}

export LESS='-FRX'
alias grep='grep --color'
alias ls='gls --color'
alias la='ls -lart'
alias cp='cp -i'
alias mv='mv -i'
alias bp='emacs ~/.bash_profile'

function dircolors() { gdircolors $@; }
eval $(require ~/.dircolors dircolors)

export GIT_PS1_SHOWDIRTYSTATE=true
export GIT_PS1_SHOWSTASHSTATE=true
export GIT_PS1_SHOWUNTRACKEDFILES=true
export GIT_PS1_SHOWUPSTREAM=true
export PS1='$(virtual_env_prompt_prefix)[\u@\h \W$(__git_ps1 " (%s)")]\$ '

function gitd() {
    dir=$1 && shift
    git --work-tree=$dir --git-dir=$dir/.git $@
}

alias cdgit='cd "$(git rev-parse --show-toplevel)"'

export CODE_DIR="$HOME/code"

function multi_pull() {
    local failures
    for p in $@; do
	echo pulling $p
	pushd . >/dev/null
	cd $p
	git pr
	if [ $? != 0 ]; then
	    failures+=" $(basename $p)"
	fi
	popd >/dev/null
    done
    for failure in $failures; do
	echo FAILED FAILED FAILED $failure FAILED FAILED FAILED
    done
}

function pvirtualenv() {
    local name=$1 && shift
    ~/dev/penv/penv.pl ${PWORKON_HOME}/${name} $*
}

function pworkon() {
    local name=$1 && shift
    if [ -n "${name}" -a -d "${PWORKON_HOME}/${name}" ]; then
	require ${PWORKON_HOME}/${name}/bin/activate
    else
	echo "${name} is not a pvirtualenv"
    fi
}

function plsvirtualenv() {
    local saved=$(shopt -p nullglob)
    shopt -s nullglob
    for d in ${PWORKON_HOME}/*; do
	echo $(basename $d)
    done
    eval $saved
}

function pcdvirtualenv() {
    local name=$1 && shift
    if [ -n "${name}" -a -d "${PWORKON_HOME}/${name}" ]; then
	cd ${PWORKON_HOME}/${name}
    elif [ -z "${name}" ]; then
	cd ${PWORKON_HOME}
    else
	echo "${name} is not a pvirtualenv"
    fi
}

function prmvirtualenv() {
    local name=$1 && shift
    if [ -n "${name}" -a -d "${PWORKON_HOME}/${name}" ]; then
	rm -rf ${PWORKON_HOME}/${name}
    else
	echo "${name} is not a pvirtualenv"
    fi
}

shopt -s histappend
shopt -s cmdhist
export HISTSIZE=100000
export HISTFILESIZE=100000

alias dokinit='kinit -f -r 7d -l 10h'

function fixsynergy() {
    host=${1:-office155}
    ssh $host 'net stop "Synergy Client"; net start "Synergy Client"'
}

function grepr() {
    grep -r --color --exclude-dir=.svn --exclude-dir=.git "$@"
}

function greppom() {
    grep -r --color --exclude-dir=.svn --exclude-dir=.git --include pom.xml "$@"
}

function grepsrc() {
    grep -r --color --exclude-dir=.svn --exclude-dir=.git --exclude-dir=target "$@"
}

function findsrc() {
    local paths=$1 && shift;
    while [ $# -gt 0 -a "${1##-}" == "$1" ]; do
	paths="${paths} $1" && shift
    done
    echo find ${paths} \( "$@" \) -a -not -path '*/target/*' -a -not -path '*/.git/*' -a -not -path '*/.svn/*' >&2
    find ${paths} \( "$@" \) -a -not -path '*/target/*' -a -not -path '*/.git/*' -a -not -path '*/.svn/*'
}

function src() {
    source ~/.bash_profile
}

function mci() {
    mvn clean install $*
}

function mcit() {
    mvn clean install -DskipTests $*
}

function mgs() {
    mvn clean generate-sources $*
}

function symlinks() {
    if [ $# -lt 2 ]; then
	echo "usage: dotfiles [-n] sourcedir targetdir"
	echo "options:"
	echo "	-n	dryrun"
	return
    fi

    if [ $1 == "-n" ]; then
	local dryrun="true"
	shift
    fi

    local sourcedir=$1 && shift
    local targetdir=$1 && shift

    if [ ! -d "${sourcedir}" -o ! -d "${targetdir}" ]; then
	echo both arguments should be directories
	return
    fi

    local f
    local old_ifs=${IFS}
    IFS=$'\n'
    for f in $(find ${sourcedir} -mindepth 1 -maxdepth 1 -not \( -type d -a \( -name .git -o -name .svn \) \) ); do
	local target=${targetdir}/$(basename ${f})
	if [ -L "${target}" -o -e "${target}" ]; then
	    if [ ! -L "${target}" ]; then
		echo ${target} is not a symlink
	    else
		local pointer=$(greadlink -f ${target})
		local source_pointer=$(greadlink -f ${f})
		if [ "${pointer}" != "${source_pointer}" ]; then
		    echo ${target} points to ${pointer} not ${source_pointer}
		else
		    echo ${target} already points to ${source_pointer}
		fi
	    fi
	    local diff
	    diff=$(diff -ru ${f} ${target})
	    if [ $? -ne 0 ]; then
		echo ${target} is different from ${f}:
		echo
		echo "${diff}"
	    fi
	else
	    echo linking ${f} to ${target}
	    if [ -n "${dryrun}" ]; then
		echo "gln -rs \"${f}\" \"${target}\""
	    else
		gln -rs "${f}" "${target}"
	    fi
	fi
    done
    IFS=${old_ifs}
}

function jarbomb() {
    if [ $# -lt 4 ]; then
	echo usage: jarbomb host image deployment file [file...]
	return
    fi
    host=$1 && shift;
    image=$1 && shift;
    deployment=$1 && shift;

    if [ "$image" == "trading" ]; then
	scp $@ root@$host:/home/images/${deployment}/system/lib/
    else
	scp $@ root@$host:/home/images/${image}/${deployment}/system/lib/
    fi
}

function jarclusterbomb() {
    if [ $# -lt 4 ]; then
	echo usage: jarclusterbomb host image deployment projectroot module [module...]
	return
    fi
    host=$1 && shift;
    image=$1 && shift;
    deployment=$1 && shift;
    root=$1 && shift;
    modules=$@
    
    if [ ${#modules} -eq 0 ]; then
	modules=$(find ${root} -mindepth 2 -maxdepth 2 -name target -a -type d | awk -F/ '{ print $(NF - 1) }')
    fi

    local version=$(xpath ${root}/pom.xml '/project/version/text()' 2>/dev/null)
    local tempdir=$(mktemp -d -t jarclusterbomb)
    local module
    for module in ${modules}; do
	cp ${root}/${module}/target/${module}-${version}.jar ${tempdir}/${module}.jar
    done

    jarbomb ${host} ${image} ${deployment} ${tempdir}/*
}

function crucible_scan() {
    local project=$1 && shift
    curl -v -X POST -H "X-Api-Key: 4a8f47c69991347b19bd48c0a710a21464b25224" https://crucible:6443/rest-service-fecru/admin/repositories-v1/${project}/scan
}

function gen_tags() {
    ctags -R -e . ${VIRTUAL_ENV:-} $*
}

function wol() {
    local ether=${1//:/} && shift
    local ip=$1 && shift

    wol_packet="FFFFFFFFFFFF$(printf "${ether}%.0s" {1..16})"
    echo $wol_packet | xxd -r -p | nc -c -v -u -n ${ip} 9
}

function gerritify() {
    if [ $# -lt 1 ]; then
	echo usage: gerritify gerrit_name [remote_name] [gerrit_host] [gerrit_port]
	return
    fi
    local gerrit_name=${1}
    local remote_name=${2:-gerrit}
    local gerrit_host=${3:-gerrit}
    local gerrit_port=${4:-29418}

    #local url=$(git ls-remote --get-url | python -c 'import sys; from urlparse import urlparse; print "\n".join(urlparse(line)._replace(netloc="%s:%s" % (sys.argv[1], sys.argv[2])).geturl() for line in sys.stdin)' ${gerrit_host} ${gerrit_port})

    scp -P ${gerrit_port} -p ${gerrit_host}:hooks/commit-msg .git/hooks/
    git remote add ${remote_name} ssh://${gerrit_host}:${gerrit_port}/${gerrit_name}
    git config remote.${remote_name}.push HEAD:refs/for/master
    git remote show ${remote_name}
}

require "$HOME/.rvm/scripts/rvm"

export DYLD_LIBRARY_PATH=/usr/local/lib/gcc/x86_64-apple-darwin13.4.0/4.9.2/:${DYLD_LIBRARY_PATH}
export HOMEBREW_GITHUB_API_TOKEN='dd0c8b079f06ad87f50684133a119d3ce98f7269'
export GOPATH=~/dev/go

export JAVA_HOME=$(/usr/libexec/java_home)
export MONO_GAC_PREFIX="/usr/local"

alias reload_prefs='killall cfprefsd'

require ~/.bash_imc

# makes Ctrl-O work
stty discard undef

# for snipcap

function ut() { time=$1 && shift; day_offset=$1 && shift; extra_seconds=$1 && shift; date +%s -d "$time $day_offset days ago $extra_seconds seconds"; }
function df() { local time=$1 && shift; local length=$1 && shift; local day_offset=${1:-0} && shift; echo "-a $(ut $time $day_offset 0) -b $(ut $time $day_offset $length)"; }

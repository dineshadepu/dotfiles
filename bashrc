# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
*i*) ;;
    *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
xterm-color|*-256color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
    # We have color support; assume it's compliant with Ecma-48
    # (ISO/IEC-6429). (Lack of such support is extremely rare, and such
    # a case would tend to support setf rather than setaf.)
    color_prompt=yes
else
    color_prompt=
fi
fi

if [ "$color_prompt" = yes ]; then
PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
;;
*)
;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
alias ls='ls --color=auto'
#alias dir='dir --color=auto'
#alias vdir='vdir --color=auto'

alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
fi

# colored GCC warnings and errors
#export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
. ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi
fi

# My configuration starts
source ~/git-completion.bash
# export PATH="$HOME/miniconda3/bin:$PATH"
# colors!
green="\[\033[0;32m\]"
blue="\[\033[0;34m\]"
purple="\[\033[0;35m\]"
reset="\[\033[0m\]"

# Change command prompt
source ~/git-prompt.sh
export GIT_PS1_SHOWDIRTYSTATE=1
# '\u' adds the name of the current user to the prompt
# '\$(__git_ps1)' adds git-related stuff
# '\W' adds the name of the current directory

export PS1=" \n \n ----CLWS HPC CLWS HPC CLWS HPC ----------------------------------\n $purple\u@UoSHPCLab$blue\$(__git_ps1)$green \$PWD $ $reset \n| $reset HPC LAB UoS => "
# PATH=/usr/racket/bin:$PATH

bind '"\e[A": history-search-backward'
bind '"\e[B": history-search-forward'

# sync bash history across terminals
export HISTCONTROL=ignoredups:erasedups  # no duplicate entries
export HISTSIZE=100000                   # big big history
export HISTFILESIZE=100000               # big big history
shopt -s histappend                      # append to history, don't overwrite it

# Save and reload the history after each command finishes
export PROMPT_COMMAND="history -a; history -c; history -r; $PROMPT_COMMAND"

# fuzzy completion

alias ..="cd .."
alias ...="cd ../../"
alias l='ls -l'
alias la='ls -a'
alias sl="ls"
alias s="ls"
alias v="pysph view"
alias rmp="rm -rf"
alias open="xdg-open"
alias batp="bat -l=py"

# Paths

# export WORKON_HOME=~/.virtualenvs
export PATH=$HOME/.local/bin:$PATH
# source /home/dinesh/.local/bin/virtualenvwrapper.sh
export PATH=/usr/include:$PATH
export PATH=$HOME/opt:$PATH
export PATH=$HOME/.cargo/bin:$PATH
export PATH=/usr/bin:$PATH

export PATH=$HOME/anaconda3/bin:$PATH
# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/adepudinesh/anaconda3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/adepudinesh/anaconda3/etc/profile.d/conda.sh" ]; then
        . "/home/adepudinesh/anaconda3/etc/profile.d/conda.sh"
    else
        export PATH="/home/adepudinesh/anaconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<
conda activate pysph_env

# Zoxide for cd
# eval "$(zoxide init bash --cmd cd)"


# for emacs rtags
# export PATH=/post_doc/softwares/rtags/build/bin:$PATH

# for mesa pysph viewer problem libGL error
# export MESA_LOADER_DRIVER_OVERRIDE=i965
export LD_PRELOAD=/usr/lib/x86_64-linux-gnu/libstdc++.so.6
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/lib/x86_64-linux-gnu/dri/


# for visit visualizer
export PATH=/usr/local/visit/bin:$PATH
# for paraview visualizer
export PATH=/home/adepudinesh/post_doc/softwares/ParaView-5.12.0-MPI-Linux-Python3.10-x86_64/bin:$PATH


export PATH=/usr/local/cuda/bin:$PATH
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/cuda-12/lib64/

# for parallel hdf5 for Cabana
export HDF_INSTALL_DIR=/home/dinesh/post_doc/softwares/hdf5/
# for silo particle output
export SILO_INCLUDE_DIR=/home/adepudinesh/post_doc/softwares/Silo/include/
export SILO_LIBRARY=/home/adepudinesh/post_doc/softwares/Silo/lib/
export SILO_INSTALL_DIR=/home/dinesh/post_doc/softwares/Silo

export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/home/adepudinesh/post_doc/softwares/hdf5/src/.libs
export PATH=/home/adepudinesh/post_doc/softwares/hdf5/bin:$PATH
export PATH=/home/adepudinesh/post_doc/softwares/hdf5/src:$PATH
export PATH=/home/adepudinesh/post_doc/softwares/hdf5:$PATH
export PATH=/home/adepudinesh/post_doc/softwares/hdf5/src/.libs:$PATH
export HDF5_ROOT=/home/adepudinesh/post_doc/softwares/hdf5
export HDF5_SRC=/home/adepudinesh/post_doc/softwares/hdf5/src/

# Cabana installation paths
export KOKKOS_SRC_DIR=/home/adepudinesh/post_doc/softwares/kokkos
export Kokkos_DIR=/home/adepudinesh/post_doc/softwares/kokkos/build
export KOKKOS_INSTALL_DIR=$KOKKOS_SRC_DIR/build/install
export CABANA_INSTALL_DIR=/home/adepudinesh/post_doc/softwares/Cabana/build/install


# Cabana installation paths for CPU
export KOKKOS_INSTALL_DIR_CPU=$KOKKOS_SRC_DIR/cpu_build/install
export CABANA_INSTALL_DIR_CPU=/home/adepudinesh/post_doc/softwares/Cabana/cpu_build/install



export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/home/adepudinesh/.local/netcdf/lib/

# If you come from bash you might have to change your $PATH.

# Path to your oh-my-zsh installation.
# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
export ZSH=~/.oh-my-zsh
#ZSH_THEME="refined"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git osx vi-mode dirstack haskell wd)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
export SSH_KEY_PATH="~/.ssh/rsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
alias zshconfig="nvim ~/.zshrc"
alias :q="exit"
alias :r="source ~/.zshrc"
alias my-ghci="$HOME/Dev/ghc/inplace/bin/ghc-stage2 --interactive "
alias head-ghci="$HOME/Dev/ghc-head/inplace/bin/ghc-stage2 --interactive "
export PAGER="less"

PROMPT='λ '
export EDITOR='nvim'
export REFINED_PROMPT_SYMBOL='λ'

export PATH=/bin
export PATH=$PATH:/Applications/CoqIDE_8.6.1.app/Contents/Resources/bin
export PATH=$PATH:/$HOME/Dev/purescript/purescript/.stack-work/install/x86_64-osx/nightly-2017-09-10/8.2.1/bin
#export PATH=$PATH:/$HOME/Dev/purescript/purescript/.stack-work/install/x86_64-osx/lts-8.5/8.0.2/bin
export PATH=$PATH:/usr/local/bin
export PATH=$PATH:/usr/local/Cellar/emacs/25.2/bin
export PATH=$PATH:/usr/bin
export PATH=$PATH:/usr/sbin
export PATH=$PATH:~/.cabal/bin
export PATH=$PATH:~/.local/bin
export PATH=$PATH:~/.stack/programs/x86_64-osx/ghc-8.2.1/bin
export PATH=$PATH:~/.stack/programs/x86_64-osx/ghc-8.0.2/bin
export PATH=$PATH:~/.stack/programs/x86_64-osx/ghc-7.10.3/bin
export PATH=$PATH:~/habito-web/node_modules/purescript/bin
export PATH=$PATH:/Library/TeX/texbin
export PATH=$PATH:~/.cargo/bin
export PATH=$HOME/bin:/usr/local/bin:$PATH
export PATH=$PATH:/Library/Frameworks/Mono.framework/Versions/Current/Commands
export PATH="$HOME/Library/Haskell/bin:$PATH"
export PATH=$PATH:~/.stack/programs/x86_64-osx/ghc-8.2.2/bin

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

alias shci='stack ghci $(find . -name "*.cabal" | perl -lpe "s/.*\/(.*)\.cabal/\1/g" | fzf)'
alias import='ag --nocolor --nogroup --nofilename "import qualified (\S+)(\s)+as" | perl -lpe "s/ +/ /g" | grep -v "^--" | sort | uniq | fzf | pbcopy'
alias tags='fast-tags . -R'
alias vim='nvim'
alias vi='/usr/local/Cellar/vim/8.0.0596/bin/vim'
export PATH="/usr/local/opt/libxml2/bin:$PATH"

export FZF_DEFAULT_COMMAND='
  (git ls-tree -r --name-only HEAD ||
   find . -path "*/\.*" -prune -o -type f -print -o -type l -print |
      sed s/^..//) 2> /dev/null'

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

tm() {
  [[ -n "$TMUX" ]] && change="switch-client" || change="attach-session"
  if [ $1 ]; then
    tmux $change -t "$1" 2>/dev/null || (tmux new-session -d -s $1 && tmux $change -t "$1"); return
  fi
  session=$(tmux list-sessions -F "#{session_name}" 2>/dev/null | fzf --exit-0) &&  tmux $change -t "$session" || echo "No sessions found."
}

export ZSH=~/.oh-my-zsh
DISABLE_AUTO_UPDATE="true"
DISABLE_AUTO_TITLE="true"
COMPLETION_WAITING_DOTS="true"
DISABLE_UNTRACKED_FILES_DIRTY="true"
plugins=(git osx vi-mode dirstack haskell wd)
source $ZSH/oh-my-zsh.sh
export SSH_KEY_PATH="~/.ssh/rsa_id"

PROMPT='λ '
export REFINED_PROMPT_SYMBOL='λ'
export PAGER="less"
export EDITOR='nvim'

# Path
export PATH=/bin
export PATH=$PATH:/Applications/CoqIDE_8.6.1.app/Contents/Resources/bin
export PATH=$PATH:/$HOME/Dev/purescript/purescript/.stack-work/install/x86_64-osx/nightly-2017-09-10/8.2.1/bin
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

# Aliases
alias zshconfig="nvim ~/.zshrc"
alias :q="exit"
alias :r="source ~/.zshrc"
alias my-ghci="$HOME/Dev/ghc/inplace/bin/ghc-stage2 --interactive "
alias head-ghci="$HOME/Dev/ghc-head/inplace/bin/ghc-stage2 --interactive "

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

ftpane() {
  local panes current_window current_pane target target_window target_pane
  panes=$(tmux list-panes -s -F '#I:#P - #{pane_current_path} #{pane_current_command}')
  current_pane=$(tmux display-message -p '#I:#P')
  current_window=$(tmux display-message -p '#I')

  target=$(echo "$panes" | grep -v "$current_pane" | fzf +m --reverse) || return

  target_window=$(echo $target | awk 'BEGIN{FS=":|-"} {print$1}')
  target_pane=$(echo $target | awk 'BEGIN{FS=":|-"} {print$2}' | cut -c 1)

  if [[ $current_window -eq $target_window ]]; then
    tmux select-pane -t ${target_window}.${target_pane}
  else
    tmux select-pane -t ${target_window}.${target_pane} &&
    tmux select-window -t $target_window
  fi
}

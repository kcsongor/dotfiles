export ZSH=~/.oh-my-zsh
DISABLE_AUTO_UPDATE="true"
DISABLE_AUTO_TITLE="true"
DISABLE_LS_COLORS="true"
COMPLETION_WAITING_DOTS="true"
DISABLE_UNTRACKED_FILES_DIRTY="true"
plugins=(git osx vi-mode dirstack haskell wd)
source $ZSH/oh-my-zsh.sh
source ~/.profile
export SSH_KEY_PATH="~/.ssh/rsa_id"

PROMPT='λ '
export REFINED_PROMPT_SYMBOL='λ'
export PAGER="less"
export EDITOR='nvim'

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

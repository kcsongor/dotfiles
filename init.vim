"-- PLUGIN MANAGER -------------------------------------------------------------
call plug#begin('~/.config/nvim/plugged')
" ----- * Languages
Plug 'neovimhaskell/haskell-vim'
" Plug 'eagletmt/ghcmod-vim'
Plug 'derekwyatt/vim-scala'
Plug 'dleonard0/pony-vim-syntax'
Plug 'guns/vim-sexp'
Plug 'rust-lang/rust.vim'
Plug 'Quramy/tsuquyomi'
Plug 'raichoo/purescript-vim'
Plug 'idris-hackers/idris-vim'
" ----- * Misc
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
Plug 'junegunn/goyo.vim'
Plug 'junegunn/limelight.vim'
" Plug 'Shougo/vimproc.vim'
Plug 'benekastah/neomake'
Plug '/usr/local/opt/fzf'
Plug 'junegunn/fzf.vim'
Plug 'tpope/vim-fireplace'
Plug 'tpope/vim-fugitive'
Plug 'tommcdo/vim-fubitive'
" Plug 'ctrlpvim/ctrlp.vim'
Plug 'kcsongor/vim-monochrome'

" ----- * Coq
Plug 'def-lkb/vimbufsync'
Plug 'the-lambda-church/coquille'
Plug 'jvoorhis/coq.vim'
call plug#end()

"-- MISC SETTINGS --------------------------------------------------------------
syntax on
set cursorline
set splitright
set splitbelow
set nobackup
let &shell = "zsh"
set noswapfile
set scrolloff=5
set colorcolumn=80
set autoindent
set backspace=indent,eol,start
set ignorecase
set smartcase
let mapleader = "\<Space>"
let maplocalleader = "\<Space>"
colorscheme monochrome
filetype indent off

set wildignore+=*/tmp/*,*.so,*.swp,*.zip

" Always return to terminal in insert mode
autocmd BufWinEnter,WinEnter term://* startinsert

set sts=2
set et
set sw=2

" Display tabs and trailing spaces
set listchars=tab:>~,nbsp:_,trail:.
set list

" vertical split in diff
set diffopt+=vertical

set statusline=[%n]\ %<%.99f\ %h%w%m%r%y%=%-16(\ %c\ %)

"-- LINE NUMBERS ---------------------------------------------------------------
set nu
set rnu
au WinLeave * :set nornu
au WinEnter * :call RelativeNumber()

"-- MAPPINGS -------------------------------------------------------------------

noremap <left>  <nop>
noremap <right> <nop>
noremap <up>    <nop>
noremap <down>  <nop>

inoremap <left>  <nop>
inoremap <right> <nop>
inoremap <up>    <nop>
inoremap <down>  <nop>

inoremap <C-h> <left>
inoremap <C-j> <down>
inoremap <C-k> <up>
inoremap <C-l> <right>

" Terminal normal mode on C-b
tnoremap <C-b> <C-\><C-n>

" Window movement in terminal mode
tnoremap <C-w> <C-\><C-n><C-w>

" Switch tabs
map <C-l> :tabn<cr>
map <C-h> :tabp<cr>
inoremap <C-b><C-l> <C-\><C-n>:tabn<cr>
inoremap <C-b><C-h> <C-\><C-n>:tabp<cr>

" tmux-like terminal splitting
tnoremap <C-w><Bar> <C-\><C-n>:vsp term:///bin/zsh<cr>
tnoremap <C-w>- <C-\><C-n>:sp term:///bin/zsh<cr>
nnoremap <C-w><Bar> :vsp<cr>:terminal<cr>
nnoremap <C-w>- :sp<cr>:terminal<cr>

nnoremap <Leader>uu yypVr-
nnoremap <Leader>u= yypVr=
nnoremap <Leader>u~ yypVr~
vnoremap <Leader>u "1yhv0yopVr $"1p0wv$r^

" Call the pastebin function with selection
vnoremap <silent> t :call Termbin()<cr>

" Go line-by-line on wrapped lines too
" nmap j gj
" nmap k gk

" Save file
nnoremap <Leader>tn :tabnew<CR>
nnoremap <Leader>tc :tabclose<CR>

nmap <silent> <leader>ev :e $MYVIMRC<CR>
nmap <silent> <leader>sv :so $MYVIMRC<CR>

" Buffer switch
nmap <Leader><Tab> :b 

" Toggle relative number
nmap <Leader>tr :call ToggleRelativeNumber()<CR>

" Cd to current file
nmap <Leader>cd :cd %:h<cr>

nnoremap <Leader>g :Goyo<cr>
nnoremap <Leader>ll :Limelight!! 0.4 <cr>

function! s:goyo_enter()
  silent !tmux set status off
  silent !tmux list-panes -F '\#F' | grep -q Z || tmux resize-pane -Z
  set noshowmode
  set noshowcmd
  set scrolloff=999
  Limelight 0.4
endfunction

function! s:goyo_leave()
  silent !tmux set status on
  silent !tmux list-panes -F '\#F' | grep -q Z && tmux resize-pane -Z
  set showmode
  set showcmd
  set scrolloff=5
  Limelight!
endfunction

autocmd! User GoyoEnter nested call <SID>goyo_enter()
autocmd! User GoyoLeave nested call <SID>goyo_leave()

"-- FUNCTIONS ------------------------------------------------------------------
function! Termbin() range
  echo system('echo '.shellescape(join(getline(a:firstline, a:lastline), "\\n")).'| sed s/\\\\\\\\\$// | nc termbin.com 9999 | pbcopy')
  echo "URL copied to clipboard"
endfunction

vnoremap <silent> t :call Termbin()<cr>

function! ToggleRelativeNumber()
    if &rnu
        set nornu
    else
        set rnu
    endif
endfunction

function! SendGHCI(file)
    let bnr = bufwinnr("ghci")
    if bnr > 0
      :exe bnr . "wincmd w"
    else
      vnew | :call termopen("stack exec -- ghci " . a:file) | :startinsert
    endif
endfunction

function! SendCORE(file)
    let bnr = bufwinnr("ghc-core")
    if bnr > 0
      :exe bnr . "wincmd w"
    else
      vnew | :call termopen("ghc-core --no-cast -- -dsuppress-var-kinds -dsuppress-type-applications -dsuppress-uniques " . a:file) | :startinsert
    endif
endfunction

function! ReloadGHCI()
    let bnr = bufwinnr("ghci")
    let cur = bufwinnr("%")
    if bnr > 0
      :exe bnr . "wincmd w"
      :startinsert
      :call feedkeys("\<C-l>:r\<cr>\<Esc>\<C-\>\<C-n>:".cur."wincmd w\<cr>h")
      ":exe cur . "wincmd w"
    endif
endfunction

autocmd FileType haskell nnoremap <silent> <Leader>r :w<cr> :call ReloadGHCI()<cr>

function! RelativeNumber()
    if &number
        set rnu
    else
        set nornu
    endif
endfunction

" -- Ag search
function! s:ag_to_qf(line)
  let parts = split(a:line, ':')
  return {'filename': parts[0], 'lnum': parts[1], 'col': parts[2],
        \ 'text': join(parts[3:], ':')}
endfunction

function! s:ag_handler(lines)
  if len(a:lines) < 2 | return | endif

  let cmd = get({'ctrl-x': 'split',
               \ 'ctrl-v': 'vertical split',
               \ 'ctrl-t': 'tabe'}, a:lines[0], 'e')
  let list = map(a:lines[1:], 's:ag_to_qf(v:val)')

  let first = list[0]
  execute cmd escape(first.filename, ' %#\')
  execute first.lnum
  execute 'normal!' first.col.'|zz'

  if len(list) > 1
    call setqflist(list)
    copen
    wincmd p
  endif
endfunction

command! -nargs=* Ag call fzf#run({
\ 'source':  printf('ag --nogroup --column --color "%s"',
\                   escape(empty(<q-args>) ? '^(?=.)' : <q-args>, '"\')),
\ 'sink*':    function('<sid>ag_handler'),
\ 'options': '--ansi --expect=ctrl-t,ctrl-v,ctrl-x --delimiter : --nth 4.. '.
\            '--multi --bind=ctrl-a:select-all,ctrl-d:deselect-all '.
\            '--color hl:68,hl+:110 '.
\            '--reverse '.
\            '--preview="tail \$(echo {} | gsed -r \"s/(.*):([0-9]+):[0-9]+:.*$/ -n +\2 \1/\" ) | head -"$(tput lines) ',
\ 'down':    '50%'
\ })
" -- End ag search

set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.class,*.sjsir

"-- PLUGINS --------------------------------------------------------------------

let g:indent_guides_start_level = 1
let g:indent_guides_auto_colors = 0
autocmd VimEnter,Colorscheme * :hi IndentGuidesOdd  ctermbg=233
autocmd VimEnter,Colorscheme * :hi IndentGuidesEven ctermbg=234

" autocmd! BufWritePost * Neomake

let g:ctrlp_custom_ignore = 'node_modules\|DS_Store\|build\|git\|bower_components'

nnoremap <silent> <Leader>p :CtrlPCurWD <cr>
nnoremap <silent> <Leader>a :Ag <cr>
noremap <C-p> :FZF -m --reverse --preview="head -"$(tput lines)" {}"<CR>

"-- FILETYPE SPECIFIC ----------------------------------------------------------
autocmd FileType clojure nnoremap <silent> <Leader>ef :Eval<cr>
autocmd FileType clojure nnoremap <silent> <Leader>eb :%Eval<cr>
autocmd FileType clojure inoremap <silent> <C-e> <Esc>:Eval<cr>

" Haskell
autocmd FileType haskell nnoremap <silent> <Leader>sb :call SendGHCI(@%)<cr>
autocmd FileType haskell nnoremap <silent> <Leader>sc :call SendCORE(@%)<cr>
autocmd FileType haskell nnoremap <silent> <Leader>tt :GhcModType<cr>
autocmd FileType haskell nnoremap <silent> <Leader>ti :GhcModTypeInsert<cr>
autocmd FileType haskell nnoremap <silent> <Leader>d  :GhcModSigCodegen<cr>
autocmd FileType haskell nnoremap <silent> <Leader>c  :GhcModSplitFunCase<cr>
" autocmd BufWritePost *.hs silent :GhcModCheckAndLintAsync

autocmd FileType coq nnoremap <Leader>cn :CoqNext<cr>
autocmd FileType coq nnoremap <Leader>cc :CoqToCursor<cr>
autocmd FileType coq nnoremap <Leader>cu :CoqUndo<cr>
autocmd FileType coq nnoremap <Leader>cq iProof. reflexivity. Qed.<C-c>:CoqToCursor<cr>

set diffopt+=iwhite
set diffexpr=DiffW()
function DiffW()
  let opt = ""
   if &diffopt =~ "icase"
     let opt = opt . "-i "
   endif
   if &diffopt =~ "iwhite"
     let opt = opt . "-w " " swapped vim's -b with -w
   endif
   silent execute "!diff -a --binary " . opt .
     \ v:fname_in . " " . v:fname_new .  " > " . v:fname_out
endfunction

"-- PLUGIN MANAGER -------------------------------------------------------------
call plug#begin('~/.config/nvim/plugged')
" ----- * Languages
Plug 'neovimhaskell/haskell-vim'
" Plug 'parsonsmatt/intero-neovim'
Plug 'eagletmt/ghcmod-vim'
Plug 'idris-hackers/idris-vim'
Plug 'neovimhaskell/haskell-vim'
Plug 'raichoo/purescript-vim'
" ----- * Misc
Plug '/usr/local/opt/fzf'
Plug 'Shougo/vimproc.vim'
Plug 'airblade/vim-gitgutter'
Plug 'godlygeek/tabular/'
Plug 'junegunn/fzf.vim'
Plug 'junegunn/goyo.vim'
Plug 'junegunn/gv.vim'
Plug 'junegunn/limelight.vim'
Plug 'junegunn/vim-easy-align'
Plug 'junegunn/vim-journal'
Plug 'kcsongor/vim-monochrome'
Plug 'godlygeek/tabular/'
Plug 'easymotion/vim-easymotion'
Plug 'ndmitchell/ghcid', { 'rtp': 'plugins/nvim' }
Plug 'scrooloose/nerdtree'
Plug 'tpope/vim-fugitive'
" ----- * Coq
Plug 'def-lkb/vimbufsync'
Plug 'jvoorhis/coq.vim'
Plug 'the-lambda-church/coquille'

Plug 'kcsongor/vim-monochrome-light'
call plug#end()

"-- MISC SETTINGS --------------------------------------------------------------
syntax on
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
colorscheme monochrome-light
filetype indent off

set wildignore+=*/tmp/*,*.so,*.swp,*.zip
set iskeyword+=-

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

set foldmethod=indent
set foldlevel=20
set sessionoptions+=tabpages,globals

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

nmap <silent> <leader>ev :e `=resolve(expand($MYVIMRC))`<CR>
nmap <silent> <leader>sv :so $MYVIMRC<CR>

" Buffer switch
nmap <Leader><Tab> :Buffers<cr>

" Toggle relative number
nmap <Leader>tr :call ToggleRelativeNumber()<CR>

" Cd to current file
nmap <Leader>cd :cd %:h<cr>

nnoremap <Leader>go :Goyo<cr>
nnoremap <Leader>ll :Limelight!! 0.4 <cr>

let g:goyo_width=120

function! s:goyo_enter()
  silent !tmux set status off
  silent !tmux list-panes -F '\#F' | grep -q Z || tmux resize-pane -Z
  set noshowmode
  set noshowcmd
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
    let bnr = bufwinnr("repl")
    if bnr > 0
      :exe bnr . "wincmd w"
    else
      vnew | :call termopen("stack repl " . a:file) | :startinsert
    endif
endfunction

function! SendGHCITarget(path)
    let bnr = bufwinnr("ghci")
    if bnr > 0
      :exe bnr . "wincmd w"
    else
      let cabals = split(globpath(a:path, '*.cabal'), '\n')
      let dir    = fnamemodify(a:path, ':h')
      if (dir != '/')
        if (len(cabals) == 0)
          call SendGHCITarget(dir)
        else
          vnew | :call termopen("stack ghci " . fnamemodify(cabals[0], ':h')) | :startinsert
        endif
      else
        echo "Not a cabal project"
      endif
    endif
endfunction

function! SendCORE(file)
    let bnr = bufwinnr("ghc-core")
    if bnr > 0
      :exe bnr . "wincmd w"
    else
      vnew | :call termopen("stack exec -- ghc-core --no-cast -- -dsuppress-var-kinds -dsuppress-type-applications -dsuppress-uniques " . a:file) | :startinsert
    endif
endfunction

function! ReloadGHCI()
    let bnr = bufwinnr("repl")
    let cur = bufwinnr("%")
    if bnr > 0
      :exe bnr . "wincmd w"
      :startinsert
      :call feedkeys("\<C-l>:r\<cr>\<Esc>\<C-\>\<C-n>:".cur."wincmd w\<cr>h")
      ":exe cur . "wincmd w"
    endif
endfunction

autocmd FileType haskell nnoremap <silent> <Leader>r :w<cr> :call ReloadGHCI()<cr>

function! EditCabal(path)
  let cabals = split(globpath(a:path, '*.cabal'), '\n')
  let dir    = fnamemodify(a:path, ':h')
  if (dir != '/')
    if (len(cabals) == 0)
      call EditCabal(dir)
    else
      execute 'edit' cabals[0]
    endif
  else
    echo "No cabal file found"
  endif
endfunction

autocmd FileType haskell nnoremap <silent> <leader>ec :call EditCabal(expand('%:p'))<cr>

function! InsertHabitoModuleName(path)
  let mname  = fnamemodify(a:path, ":s?.*/Habito?Habito?:r:gs?/?.?")
  exe ':call append(0, "module ' . mname . ' where")'
endfunction

autocmd FileType haskell nnoremap <silent> <leader>mi :call InsertHabitoModuleName(expand('%:p'))<cr>

let s:ghc_cached_language_pragmas
  \= sort(split(system('ghc --supported-languages'), '\n'))

for lp in s:ghc_cached_language_pragmas
  exe 'amenu GHC_LANGUAGES.' . lp . ' :call append(0, "{-# LANGUAGE ' . lp . ' #-}")<cr>'
endfor

function! LoadCabal()
  let s:stack_pkgs
    \= sort(split(system('stack exec -- ghc-pkg list --simple-output'), ' '))

  for lp in s:stack_pkgs
    let pkgname = matchstr(lp, '.*-')[0:-2]
    exe 'amenu GHC_PACKAGES.' . pkgname . ' :call append((search("build-depends", "n") + 1), repeat(" ", cindent(search("build-depends", "n"))) . ", ' . pkgname . '")<cr>'
  endfor
endfunction

autocmd FileType cabal :call LoadCabal()
autocmd FileType cabal noremap <LocalLeader>ci :emenu GHC_PACKAGES.

autocmd FileType haskell noremap <LocalLeader>la :emenu GHC_LANGUAGES.
autocmd FileType haskell noremap <LocalLeader>sl mzgg:Tabularize/#-}<CR>vip:sort<CR>`z
autocmd FileType haskell noremap <LocalLeader>si mz:Tabularize/as<CR>vip:sort<CR>`z

function! RelativeNumber()
    if &number
        set rnu
    else
        set nornu
    endif
endfunction

set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.class,*.sjsir

"-- PLUGINS --------------------------------------------------------------------
let g:indent_guides_start_level = 1
let g:indent_guides_auto_colors = 0
autocmd VimEnter,Colorscheme * :hi IndentGuidesOdd  ctermbg=233
autocmd VimEnter,Colorscheme * :hi IndentGuidesEven ctermbg=234

" Show highlight group
nnoremap <leader>hg :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<'
\ . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
\ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>


"-- FILETYPE SPECIFIC ----------------------------------------------------------

" Haskell
autocmd FileType haskell nnoremap <silent> <Leader>sb :call SendGHCI(@%)<cr>
autocmd FileType haskell nnoremap <silent> <Leader>st :call SendGHCITarget(expand('%:p'))<cr>
autocmd FileType haskell nnoremap <silent> <Leader>sc :call SendCORE(@%)<cr>
autocmd FileType haskell nnoremap <silent> <Leader>tt :GhcModType<cr>
autocmd FileType haskell nnoremap <silent> <Leader>ti :GhcModTypeInsert<cr>
autocmd FileType haskell nnoremap <silent> <Leader>d  :GhcModSigCodegen<cr>
autocmd FileType haskell nnoremap <silent> <Leader>c  :GhcModSplitFunCase<cr>

autocmd FileType haskell nnoremap <silent> <Leader>li O<esc>80i-<esc>
autocmd FileType haskell vnoremap <silent> <Leader>bb :'<,'>!brittany<cr>

autocmd FileType coq nnoremap <Leader>cn :CoqNext<cr>
autocmd FileType coq nnoremap <Leader>cc :CoqToCursor<cr>
autocmd FileType coq nnoremap <Leader>cu :CoqUndo<cr>

set diffopt+=iwhite
set diffexpr=DiffW()
function! DiffW()
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
"-- Misc plugins and mappings --------------------------------------------------
xmap ga <Plug>(EasyAlign)

nmap ga <Plug>(EasyAlign)

nnoremap <leader>a :Ag!<cr>
nnoremap <leader>bc :BCommits<cr>
nnoremap <leader>gc :Commits<cr>
nnoremap <leader>gf :GFiles?<cr>
nnoremap <leader>bl :BLines<cr>
nnoremap <leader>gg :GitGutterLineHighlightsToggle<cr>
nnoremap <leader>gb :Gblame<cr>
let g:gitgutter_diff_args = '-w'
nnoremap <leader><Tab> :Buffers<cr>
nnoremap <leader><Enter> :Commands<cr>
noremap <C-p> :FZF<cr>
nnoremap <leader>gt :call fzf#vim#tags(expand('<cword>'), {'options': '--exact --select-1 --exit-0'})<CR>
nnoremap <leader>t :Tabularize/
let g:fzf_buffers_jump = 1

nnoremap <leader>gf :call fzf#vim#ag("module " . expand('<cWORD>'))<cr>

let g:fzf_colors =
\ { 'fg':      ['fg', 'Comment'],
  \ 'bg':      ['bg', 'Normal'],
  \ 'hl':      ['fg', 'String'],
  \ 'fg+':     ['fg', 'Comment', 'String', 'Normal'],
  \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
  \ 'hl+':     ['fg', 'Statement'],
  \ 'info':    ['fg', 'PreProc'],
  \ 'prompt':  ['fg', 'Conditional'],
  \ 'pointer': ['fg', 'Exception'],
  \ 'marker':  ['fg', 'Keyword'],
  \ 'spinner': ['fg', 'Label'],
  \ 'header':  ['fg', 'Comment'] }

command! -bang -nargs=* Ag
  \ call fzf#vim#ag(<q-args>,
  \                 <bang>0 ? fzf#vim#with_preview('up:60%')
  \                         : fzf#vim#with_preview('right:50%:hidden', '?'),
  \                 <bang>0)

command! -bang -nargs=* Rg
  \ call fzf#vim#grep(
  \   'rg --column --line-number --no-heading --color=always '.shellescape(<q-args>), 1,
  \   <bang>0 ? fzf#vim#with_preview('up:60%')
  \           : fzf#vim#with_preview('right:50%:hidden', '?'),
  \   <bang>0)

function! s:fzf_statusline()
  highlight fzf1 ctermfg=161 ctermbg=251
  highlight fzf2 ctermfg=23 ctermbg=251
  highlight fzf3 ctermfg=237 ctermbg=251
  setlocal statusline=%{getcwd()}%#fzf1#\ >\ %#fzf2#fz%#fzf3#f
endfunction

autocmd! User FzfStatusLine call <SID>fzf_statusline()

nnoremap <leader>x( di(va(p``
nnoremap <leader>x[ di[va[p``
nnoremap <leader>x{ di{va{p``

function! ChooseBuffer (buffername)
  let bnr = bufwinnr(a:buffername)
  if bnr > 0
    :exe bnr . "wincmd w"
  else
    echo a:buffername . ' does not exist'
    silent execute 'split ' . a:buffername
  endif
endfunction

"-- SYNTAX HASKELL -----------------------------------------------------------

"let g:haskell_enable_quantification = 1
"let g:haskell_enable_recursivedo = 1
"let g:haskell_enable_arrowsyntax = 1
"let g:haskell_enable_pattern_synonyms = 1
"let g:haskell_enable_typeroles = 1
"let g:haskell_enable_static_pointers = 1
"let g:haskell_backpack = 1
let g:haskell_disable_TH = 1

"-- CONCEAL ------------------------------------------------------------------
syntax match hsNiceOperator "\/=" conceal cchar=≠

function! ToggleConcealQualified()
  if (matchdelete(99) == -1)
    call matchadd('Conceal', '\(qualified\|import\|as\)\@<![^a-zA-Z0-9\.]\zs\([A-Z]\w*\.\)\+', 100, 99)
  endif
endfunction

nnoremap <silent> <leader>cc :silent! call ToggleConcealQualified()<cr>

"autocmd! InsertEnter * :set conceallevel=0
"autocmd! InsertLeave * :set conceallevel=2
set concealcursor=nvic


"-- iPad BINDINGS -------------------------------------------------------------
nnoremap <leader>w <C-w>
nnoremap <leader>tn :tabnew<cr>
nnoremap <leader>ll :tabn<cr>
nnoremap <leader>hh :tabp<cr>

"-- HIGHLIGHTS ---------------------------------------------------------------
hi DiffAdd    ctermfg=white   ctermfg=35 ctermbg=232
hi DiffChange ctermfg=blue    ctermbg=black
hi DiffDelete ctermfg=1       ctermbg=black
hi DiffText   ctermfg=yellow  ctermbg=black

"-- HIGHLIGHTS ---------------------------------------------------------------
nnoremap <silent> <leader>t :silent !tmux send-keys -ttall:zsh.0 "clear && stack build --fast" C-m<cr>
vnoremap <leader>wed :'<,'>normal we D<cr>

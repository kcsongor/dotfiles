"-- PLUGIN MANAGER -------------------------------------------------------------
call plug#begin('~/.config/nvim/plugged')
" ----- * Languages
Plug 'neovimhaskell/haskell-vim'
" Plug 'parsonsmatt/intero-neovim'
Plug 'FrigoEU/psc-ide-vim'
Plug 'eagletmt/ghcmod-vim'
Plug 'idris-hackers/idris-vim'
Plug 'purescript-contrib/purescript-vim'
" ----- * Misc
Plug '/usr/local/opt/fzf'
Plug 'Shougo/vimproc.vim'
Plug 'airblade/vim-gitgutter'
Plug 'godlygeek/tabular/'
Plug 'junegunn/fzf.vim'
Plug 'junegunn/gv.vim'
Plug 'junegunn/vim-easy-align'
Plug 'kcsongor/vim-monochrome'
Plug 'kcsongor/vim-monochrome-light'
Plug 'kcsongor/vim-hs'
Plug 'godlygeek/tabular/'
Plug 'easymotion/vim-easymotion'
Plug 'ndmitchell/ghcid', { 'rtp': 'plugins/nvim' }
Plug 'scrooloose/nerdtree'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rhubarb'
Plug 'tpope/vim-dispatch'
Plug 'sjl/gundo.vim'
" ----- * Coq
Plug 'def-lkb/vimbufsync'
Plug 'jvoorhis/coq.vim'
Plug 'the-lambda-church/coquille'

Plug 'kcsongor/vim-monochrome-light'
Plug 'wakatime/vim-wakatime'
Plug 'owickstrom/vim-colors-paramount'
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
filetype indent off

"if strftime("%H") < 19 && strftime("%H") > 10
"  colorscheme monochrome-light
"else
"  colorscheme monochrome
"endif
colorscheme monochrome

set wildignore+=*/tmp/*,*.so,*.swp,*.zip
set iskeyword+=-

autocmd! bufwritepost init.vim source %
autocmd! bufwritepost haskell.vim source %

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

set statusline=%r%y\ %=\ %m\ %f

silent! set winheight=30
silent! set winminheight=5
silent! set winwidth=80
silent! set winminwidth=10

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

" select last pasted text
nnoremap <expr> gb '`[' . strpart(getregtype(), 0, 1) . '`]'

" Switch tabs
map <C-l> :tabn<cr>
map <C-h> :tabp<cr>
inoremap <C-b><C-l> <C-\><C-n>:tabn<cr>
inoremap <C-b><C-h> <C-\><C-n>:tabp<cr>

if (has('nvim'))
  " tmux-like terminal splitting
  tnoremap <C-w><Bar> <C-\><C-n>:vsp term:///bin/zsh<cr>
  tnoremap <C-w>- <C-\><C-n>:sp term:///bin/zsh<cr>
  " Terminal normal mode on C-b
  tnoremap <C-b> <C-\><C-n>

  nnoremap <C-w><Bar> :vsp<cr>:terminal<cr>
  nnoremap <C-w>- :sp<cr>:terminal<cr>

  " Window movement in terminal mode
  tnoremap <C-w> <C-\><C-n><C-w>
endif

nnoremap <Leader>uu yypVr-
nnoremap <Leader>u= yypVr=
nnoremap <Leader>u~ yypVr~
vnoremap <Leader>u "1yhv0yopVr $"1p0wv$r^

" Call the pastebin function with selection
vnoremap <silent> t :call Termbin()<cr>

nmap <silent> <leader>ev :e `=resolve(expand($MYVIMRC))`<CR>
nmap <silent> <leader>sv :so $MYVIMRC<CR>
nmap <silent> <leader>eh :call EditH()<CR>

" Buffer switch
nmap <Leader><Tab> :Buffers<cr>

" Toggle relative number
nmap <Leader>tr :call ToggleRelativeNumber()<CR>

" Cd to current file
nmap <Leader>cd :cd %:h<cr>

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

function! LoadCabal()
  let s:stack_pkgs
    \= sort(split(system('stack exec -- ghc-pkg list --simple-output'), ' '))

  for lp in s:stack_pkgs
    let pkgname = matchstr(lp, '.*-')[0:-2]
    exe 'amenu GHC_PACKAGES.' . pkgname . ' :call append((search("build-depends", "n") + 1), repeat(" ", cindent(search("build-depends", "n"))) . ", ' . pkgname . '")<cr>'
  endfor
endfunction

"autocmd FileType cabal :call LoadCabal()
"autocmd FileType cabal noremap <LocalLeader>ci :emenu GHC_PACKAGES.

function! RelativeNumber()
    if &number
        set rnu
    else
        set nornu
    endif
endfunction

set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.class,*.sjsir,*.o,*.hi

"-- PLUGINS --------------------------------------------------------------------
" Show highlight group
nnoremap <leader>hg :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<'
\ . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
\ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>


"-- FILETYPE SPECIFIC ----------------------------------------------------------

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

nnoremap <leader>nf :NERDTreeFind<cr>
nnoremap <C-n> :NERDTree<cr>

nnoremap <leader>a :Ag!<cr>
nnoremap <leader>f "oyw :Ag! <C-R><C-W><cr>
vnoremap <leader>f "oy :Ag! <C-R>o<cr>
nnoremap <leader>F :Ag! <C-R>o<cr>
nnoremap <C-J> :lnext<cr>
nnoremap <C-K> :lprev<cr>
nnoremap <leader>bc :BCommits<cr>
nnoremap <leader>gc :Commits<cr>
"nnoremap <leader>gf :GFiles?<cr>
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

function! EditH()
  :call fzf#run({
  \ 'source': reverse(split(globpath(&rtp, "*/haskell.vim"), "\n")),
  \ 'sink': 'e',
  \ 'down': '20%'})
endfunction

let g:fzf_colors =
\ { 'fg':      ['fg', 'Normal'],
  \ 'bg':      ['bg', 'Normal'],
  \ 'hl':      ['fg', 'String'],
  \ 'fg+':     ['fg', 'Normal', 'String', 'Normal'],
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

command! ColEd
  \ call fzf#run({'source': split(globpath(&rtp, 'colors/*.vim'), "\n"), 'sink': 'e'})

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
vnoremap <leader>( <esc>a)<esc>gvo<esc>i(<esc>%

nnoremap <silent> <leader>ci mz:g/as <C-R><C-W>/normal yy`z<cr>

"autocmd! InsertEnter * :set conceallevel=0
"autocmd! InsertLeave * :set conceallevel=2
set concealcursor=nvic


"-- iPad BINDINGS -------------------------------------------------------------
nnoremap <leader>w <C-w>
nnoremap <leader>tn :tabnew<cr>
nnoremap <leader>ll :tabn<cr>
nnoremap <leader>hh :tabp<cr>

"-- HIGHLIGHTS ---------------------------------------------------------------
hi DiffAdd    ctermfg=NONE    ctermbg=237
hi DiffChange ctermfg=NONE    ctermbg=NONE
hi DiffDelete ctermfg=233     ctermbg=NONE
hi DiffText   ctermfg=yellow  ctermbg=NONE

"-- HIGHLIGHTS ---------------------------------------------------------------
"nnoremap <silent> <leader>t :silent !tmux send-keys -ttall:zsh.0 "clear && stack build --fast" C-m<cr>
vnoremap <leader>wed :'<,'>normal we D<cr>

"-- TEMP PS stuff ---------------------------------------------------------------
function! PsciBuffer()
  return max([bufwinnr("pulp"), bufwinnr("repl")])
endfunction

function! ReloadPsci()
    let bnr = PsciBuffer()
    let cur = bufwinnr("%")
    if bnr > 0
      :exe bnr . "wincmd w"
      :startinsert
      :call feedkeys("\<C-l>:r\<cr>\<Esc>\<C-\>\<C-n>:".cur."wincmd w\<cr>h")
    endif
endfunction

function! s:send_psci(file)
    let bnr = PsciBuffer()
    if bnr > 0
      :exe bnr . "wincmd w"
    else
      vnew | :call termopen("pulp repl") | :startinsert
    endif
endfunction

nnoremap <silent> <leader>pb :call <SID>send_psci(@%)<cr>
nnoremap <silent> <leader>r :w<cr> :call ReloadPsci()<cr>

nnoremap <silent> <leader>gw :silent! call FindAllUsage(expand('<cword>'))<cr>
nnoremap <silent> <leader>ga :silent! call FindAllUsage()<cr>

function! FindAllUsage(...)
  call matchdelete(66)
  let word = 0 < a:0 ? a:1 : inputdialog("Word to search for: ")
  hi FoundGroup ctermbg=blue ctermfg=white
  exe "Glgrep! -w " . shellescape(word)
  ldo call matchadd('FoundGroup', '\<' . word . '\>', 100, 66)
endfunction

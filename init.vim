"-- PLUGIN MANAGER -------------------------------------------------------------
call plug#begin('~/.config/nvim/plugged')
" ----- * Languages
Plug 'neovimhaskell/haskell-vim'
Plug 'idris-hackers/idris-vim'
Plug 'neomake/neomake'
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
Plug 'ndmitchell/ghcid', { 'rtp': 'plugins/nvim' }
Plug 'scrooloose/nerdtree'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rhubarb'
Plug 'tpope/vim-dispatch'
Plug 'sjl/gundo.vim'
Plug 'lervag/vimtex'
" ----- * Coq
Plug 'def-lkb/vimbufsync'
Plug 'jvoorhis/coq.vim'
Plug 'the-lambda-church/coquille'

Plug 'wakatime/vim-wakatime'
Plug 'brooth/far.vim'

Plug 'mattn/webapi-vim'
Plug 'mattn/gist-vim'
Plug 'tpope/vim-commentary'
Plug 'itchyny/vim-cursorword'
Plug 'haya14busa/incsearch.vim'
Plug 'ap/vim-buftabline'
Plug 'itchyny/vim-parenmatch'
call plug#end()

"-- MISC SETTINGS --------------------------------------------------------------
syntax on
set cursorline
set nowrap
set hidden
set splitright
set splitbelow
set nobackup
let &shell = "zsh"
set noswapfile
set scrolloff=0
set colorcolumn=80
set autoindent
set backspace=indent,eol,start
set ignorecase
set smartcase
set lazyredraw
let mapleader = "\<Space>"
let maplocalleader = "\<Space>"
filetype indent off

colorscheme monochrome

set wildignore+=*/tmp/*,*.so,*.swp,*.zip
set iskeyword+=-

autocmd! bufwritepost init.vim source %
autocmd! bufwritepost haskell.vim source %

" Always return to terminal in insert mode
"autocmd BufWinEnter,WinEnter term://* startinsert

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

set statusline=%#MyGroup2#\ %n\ %Y\ %#MyGroup0#\ %{fugitive#head()}\ %=\ %m\ %l:%c\ %L\ %#MyGroup1#\ %f

silent! set winheight=30
silent! set winminheight=5
silent! set winwidth=80
silent! set winminwidth=10

"-- LINE NUMBERS ---------------------------------------------------------------
set nu
set rnu
au WinLeave * :set nornu
"au WinEnter * :call RelativeNumber()

"-- MAPPINGS -------------------------------------------------------------------

noremap <left>  <nop>
noremap <right> <nop>
noremap <up>    <nop>
noremap <down>  <nop>

inoremap <left>  <nop>
inoremap <right> <nop>
inoremap <up>    <nop>
inoremap <down>  <nop>

nnoremap <C-c><C-d> :Gdiff<cr>

" select last pasted text
nnoremap <expr> gb '`[' . strpart(getregtype(), 0, 1) . '`]'

" Switch tabs
map <C-l> :bnext<cr>
map <C-h> :bprev<cr>
inoremap <C-b><C-l> <C-\><C-n>:bnext<cr>
inoremap <C-b><C-h> <C-\><C-n>:bprev<cr>

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

autocmd FileType tex,bib setlocal spell!
set spellfile=~/.config/nvim/en.utf-8.add
set spelllang=en

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
let g:buftabline_numbers = 2
let g:buftabline_indicators = 1
let g:buftabline_separators = 0
nmap <leader>1 <Plug>BufTabLine.Go(1)
nmap <leader>2 <Plug>BufTabLine.Go(2)
nmap <leader>3 <Plug>BufTabLine.Go(3)
nmap <leader>4 <Plug>BufTabLine.Go(4)
nmap <leader>5 <Plug>BufTabLine.Go(5)
nmap <leader>6 <Plug>BufTabLine.Go(6)
nmap <leader>7 <Plug>BufTabLine.Go(7)
nmap <leader>8 <Plug>BufTabLine.Go(8)
nmap <leader>9 <Plug>BufTabLine.Go(9)
nmap <leader>0 <Plug>BufTabLine.Go(10)

nnoremap <leader>gf :call fzf#vim#ag("module " . expand('<cWORD>'))<cr>

function! EditH()
  :call fzf#run({
  \ 'source': reverse(split(globpath(&rtp, "*/haskell.vim"), "\n")),
  \ 'sink': 'e',
  \ 'down': '20%'})
endfunction

hi MyGroup0 ctermbg=233 ctermfg=245
hi MyGroup1 ctermbg=238 ctermfg=255
hi MyGroup2 ctermbg=238 ctermfg=255

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

command! -nargs=0 Line :s/-/─/g

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
  setlocal statusline=%{getcwd()}
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

command! -nargs=1 -complete=tag FindAll silent! call FindSomeUsage(<q-args>)

nnoremap <silent> <leader>gw :silent! call FindSomeUsage(expand('<cword>'))<cr>
nnoremap <silent> <leader>ga :silent! call FindSomeUsage()<cr>
nnoremap <silent> <leader>grw :silent! call ReplaceAllWord(expand('<cword>'))<cr>
nnoremap <silent> <leader>gra :silent! call ReplaceAllWord()<cr>

nnoremap <C-j> :lnext<cr>
nnoremap <C-k> :lprev<cr>

function! FindSomeUsage(...)
  call matchdelete(66)
  let word = 0 < a:0 ? a:1 : inputdialog("Word to search for: ")
  hi FoundGroup ctermbg=blue ctermfg=white
  exe "Glgrep! -w " . shellescape(word)
  ldo call matchadd('FoundGroup', '\<' . word . '\>', 100, 66)
endfunction

function! ReplaceAllWord(...)
  call matchdelete(66)
  let word = 0 < a:0 ? a:1 : inputdialog("Word to replace: ")
  let to = 1 < a:0 ? a:2 : inputdialog("Replace (" . word . ") with: ")
  exe "Glgrep! -w " . shellescape(word)
  exe "ldo %s/\\<" . word . "\\>/" . to . "/gI \| update"
endfunction

function! ReplaceAll(...)
  let str = 0 < a:0 ? a:1 : inputdialog("String to replace: ")
  let to = 1 < a:0 ? a:2 : inputdialog("Replace (" . str . ") with: ")
  exe "Glgrep! " . shellescape(str)
  exe "ldo %s/" . str . "/" . to . "/gI \| update"
endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Showing errors

nnoremap <leader>j :cnext<cr>
nnoremap <leader>k :cprev<cr>

highlight Warning ctermfg=NONE ctermbg=240

sign define piet text=>> texthl=Error
sign define warning text=>> texthl=Warning

command! -nargs=0 MarkErrorLines call MarkErrorLines()
command! -nargs=0 ClearErrors call ClearErrors()

autocmd! QuickFixCmdPost [^l]* call MarkErrorLines()

function! ClearErrors()
  silent! call matchdelete(50)
  silent! call matchdelete(51)
  sign unplace *
endfunction

function! MarkErrorLines()
  call ClearErrors()
  let es = []
  for d in getqflist()
     if (d.lnum > 0)
       let len = strlen(split(strpart(getbufline(bufname(d.bufnr), d.lnum)[0], d.col-1))[0])
       let lsign = "piet"
       let es = es + [[d.lnum, d.col, len]]
       exe ":sign place 2 line=" . d.lnum . " name=" . lsign . " buffer=" . d.bufnr
    endif
  endfor
  call matchaddpos("Error", es, 100, 50)
endfunction

nnoremap <silent> ,, :call ClearErrors() \| :call MarkErrorLines() <cr>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"" Gist

let g:gist_open_browser_after_post = 1
let g:gist_post_private = 1

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"" Abbreviations
iabbrev \\|- ⊢
iabbrev \\|= ⊨

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"" Vimtex
let g:vimtex_view_method = 'skim'
let g:vimtex_view_automatic = 1

let g:vimtex_compiler_latexmk = {
    \ 'backend' : 'nvim',
    \ 'background' : 1,
    \ 'build_dir' : '',
    \ 'callback' : 1,
    \ 'continuous' : 1,
    \ 'executable' : 'latexmk',
    \ 'options' : [
    \   '-pdf',
    \   '-verbose',
    \   '-file-line-error',
    \   '-synctex=1',
    \   '-interaction=nonstopmode',
    \   '-use-make',
    \ ],
    \}

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""

map / <Plug>(incsearch-forward)
map ? <Plug>(incsearch-backward)
map g/ <Plug>(incsearch-stay)
let g:incsearch#auto_nohlsearch = 1
map n  <Plug>(incsearch-nohl-n)
map N  <Plug>(incsearch-nohl-N)
map *  <Plug>(incsearch-nohl-*)
map #  <Plug>(incsearch-nohl-#)
map g* <Plug>(incsearch-nohl-g*)
map g# <Plug>(incsearch-nohl-g#)

let g:loaded_matchparen = 1

let g:gist_update_on_write = 2

"-- PLUGIN MANAGER {{{
call plug#begin('~/.config/nvim/plugged')
Plug '/usr/local/opt/fzf'
Plug 'Shougo/vimproc.vim'
Plug 'SirVer/ultisnips'
Plug 'airblade/vim-gitgutter'
Plug 'ap/vim-buftabline'
Plug 'brooth/far.vim'
Plug 'def-lkb/vimbufsync'
Plug 'godlygeek/tabular/'
Plug 'haya14busa/incsearch.vim'
Plug 'idris-hackers/idris-vim'
Plug 'itchyny/vim-cursorword' " underline word under cursor
Plug 'itchyny/vim-parenmatch'
Plug 'junegunn/fzf.vim'
Plug 'junegunn/gv.vim'
Plug 'junegunn/goyo.vim'
Plug 'junegunn/vim-peekaboo'
Plug 'jvoorhis/coq.vim'
Plug 'kcsongor/vim-monochrome'
Plug 'kcsongor/vim-monochrome-light'
Plug 'ryanpcmcquen/true-monochrome_vim'
Plug 'lervag/vimtex'
Plug 'mattn/gist-vim'
Plug 'mattn/webapi-vim'
Plug 'ndmitchell/ghcid', { 'rtp': 'plugins/nvim' }
Plug 'neomake/neomake'
Plug 'neovimhaskell/haskell-vim'
Plug 'qpkorr/vim-bufkill'
Plug 'scrooloose/nerdtree'
Plug 'the-lambda-church/coquille'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rhubarb'
Plug 'wakatime/vim-wakatime'
Plug 'jiangmiao/auto-pairs'
Plug 'ervandew/supertab'
Plug 'justinmk/vim-sneak'
Plug 'benmills/vimux'
Plug 'MattesGroeger/vim-bookmarks'
Plug 'sandeepcr529/Buffet.vim'
Plug 'majutsushi/tagbar'

Plug 'kcsongor/vim-hs'
Plug 'kcsongor/vim-buffer-manager'
Plug 'kcsongor/vim-thesis-writing'
Plug 'kcsongor/vim-refactor'
Plug 'kcsongor/vim-itunes'
call plug#end()
"}}}
colorscheme monochrome
if (has('nvim'))
  syntax on
else
  set background=light
  syntax on
endif

"-- GENERAL SETTINGS {{{

au WinLeave * :set nornu
let &shell = "zsh"
let mapleader = "\<Space>"
let maplocalleader = "\<Space>"
set autoindent
set backspace=indent,eol,start
set colorcolumn=80
set cursorline
set diffopt+=vertical " vertical split in diff
set et
set foldmethod=marker
autocmd FileType c,hs setlocal foldmethod=indent
set hidden
set ignorecase
set lazyredraw
set nobackup
set noswapfile
set nowrap
set nu
set rnu
set scrolloff=0
set sessionoptions+=tabpages,globals
set smartcase
set splitbelow
set splitright
set sts=2
set sw=2
set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.class,*.sjsir,*.o,*.hi

if (has('nvim'))
  set inccommand=nosplit
endif

filetype indent off

" Display tabs and trailing spaces
set listchars=tab:>~,nbsp:_,trail:.
set list

set wildignore+=*/tmp/*,*.so,*.swp,*.zip
set iskeyword+=-

autocmd! bufwritepost init.vim source %
autocmd! bufwritepost haskell.vim source %

"}}}
"-- STATUS LINE {{{

hi Status0 ctermbg=233 ctermfg=245
hi Status1 ctermbg=238 ctermfg=255

set statusline=%#Status1#\ %n\ %Y\ %#Status0#\ %{fugitive#head()}\ %=\ %m\ %l:%c\ %L\ %#Status1#\ %f

"}}}
"-- MAPPINGS {{{
nnoremap <leader>ic :echo CurrentTrack()<cr>
map <C-l> :bnext<cr>
map <C-h> :bprev<cr>
nnoremap <C-c><C-d> :Gdiff<cr>

" select last pasted text
nnoremap <expr> gb '`[' . strpart(getregtype(), 0, 1) . '`]'

"---- terminal mode {{{
if (has('nvim'))
  tnoremap <C-w><Bar> <C-\><C-n>:vsp term:///bin/zsh<cr>
  tnoremap <C-w>- <C-\><C-n>:sp term:///bin/zsh<cr>
  "nnoremap <C-w><Bar> :vsp<cr>:terminal<cr>
  "nnoremap <C-w>- :sp<cr>:terminal<cr>
  tnoremap <C-w> <C-\><C-n><C-w>
endif
nnoremap <C-w>- :TermSplitH<cr>
nnoremap <C-w><Bar> :TermSplitV<cr>
"}}}

" Call the pastebin function with selection
vnoremap <silent> t :call Termbin()<cr>

" edit/reload .vimrc
nmap <silent> <leader>ev :e `=resolve(expand($MYVIMRC))`<CR>
nmap <silent> <leader>sv :so $MYVIMRC<CR>

" Edit haskell.vim
nmap <silent> <leader>eh :Config haskell<CR>

" Buffer switch
nmap <Leader><Tab> :Buffers<cr>

" Toggle relative number
nmap <Leader>tr :call ToggleRelativeNumber()<CR>
nnoremap <leader>t :Tabularize/

" Cd to current file
nmap <Leader>cd :cd %:h<cr>

" Locate file in nerdtree
nnoremap <leader>nf :NERDTreeFind<cr>
" Show neighbouring files in fzf
nnoremap <leader>ne :FZFNeigh<cr>
" fzf
nnoremap <leader>a :Ag!<cr>
" fzf with current word
nnoremap <leader>f "oyaw :Ag! <C-R><C-W><cr>
" fzf with selection
vnoremap <leader>f "oy :Ag! <C-R>o<cr>
" fzf with previous search term
nnoremap <leader>F :Ag! <C-R>o<cr>
nnoremap <leader>bc :BCommits<cr>
nnoremap <leader>gc :Commits<cr>
nnoremap <leader>bl :BLines<cr>
nnoremap <leader>gg :GitGutterLineHighlightsToggle<cr>
nnoremap <leader><Tab> :Buffers<cr>
nnoremap <leader><Enter> :Commands<cr>
noremap <C-p> :Files<cr>
noremap <leader>p :GFiles<cr>
nnoremap <leader>gt :call fzf#vim#tags(expand('<cword>'), {'options': '--exact --select-1 --exit-0'})<CR>

nnoremap <Left> :vertical resize -1<CR>
nnoremap <Right> :vertical resize +1<CR>
nnoremap <Up> :resize -1<CR>
nnoremap <Down> :resize +1<CR>
nnoremap <leader>80 :vertical resize 80<CR>
nnoremap <leader><leader> <C-^>

" locate haskell module under cursor
nnoremap <leader>gf :call fzf#vim#ag("^module " . expand('<cWORD>'), {'options': '-1 -0'})<cr>

"---- buftabline {{{
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
"}}}
"---- find/replace {{{
nnoremap <silent> <leader>gw :silent! call FindSomeUsage(expand('<cword>'))<cr>
nnoremap <silent> <leader>ga :silent! call FindSomeUsage()<cr>
nnoremap <leader>% :%s/\<<C-r><C-w>\>//gI\|norm``<left><left><left><left><left><left><left><left><left><left>
nnoremap <leader>< :'<,'>s/\<<C-r><C-w>\>//gI\|norm``<left><left><left><left><left><left><left><left><left><left>
nnoremap <leader>grw :call ReplaceAllWord(expand('<cword>'))<cr>
nnoremap <leader>gra :call ReplaceAll()<cr>

"}}}
"---- error navigation {{{
nnoremap <C-j> :lnext<cr>
nnoremap <C-k> :lprev<cr>

nnoremap <leader>j :cnext<cr>
nnoremap <leader>k :cprev<cr>

"nnoremap <silent> ,, :call ClearErrors() \| :call MarkErrorLines() <cr>
"}}}
"---- incsearch {{{
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
"}}}
"---- text manipulation {{{
nnoremap <leader>x( mzdi(va(p`zh
nnoremap <leader>x[ mzdi[va[p`zh
nnoremap <leader>x{ mzdi{va{p`zh

vnoremap <leader>( <esc>a)<esc>gvo<esc>i(<esc>%
vnoremap <leader>[ <esc>a]<esc>gvo<esc>i[<esc>%
vnoremap <leader>{ <esc>a}<esc>gvo<esc>i{<esc>%

nnoremap <leader>( mzdiwi(<esc>pa)<esc>`zl
nnoremap <leader>[ mzdiwi[<esc>pa]<esc>`zl
nnoremap <leader>{ mzdiwi{<esc>pa}<esc>`zl
"}}}
"---- coq {{{
autocmd FileType coq nnoremap <Leader>cn :CoqNext<cr>
autocmd FileType coq nnoremap <Leader>cc :CoqToCursor<cr>
autocmd FileType coq nnoremap <Leader>cu :CoqUndo<cr>
"}}}
"---- thesis {{{
autocmd FileType lhaskell nnoremap <leader>gS :JumpToSection<cr>
autocmd FileType lhaskell nnoremap <leader>gs :JumpToLabels section<cr>
autocmd FileType lhaskell nnoremap <leader>gl :JumpToLabels 
autocmd FileType lhaskell nnoremap <leader>lc :AddCitationFZF<cr>
"}}}
" Show highlight group
nnoremap <leader>hg :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<'
\ . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
\ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>

"}}}
"-- iPad MAPPINGS {{{

nnoremap <leader>w <C-w>
nnoremap <leader>tn :tabnew<cr>
nnoremap <leader>ll :tabn<cr>
nnoremap <leader>hh :tabp<cr>

"}}}
"-- MISC FUNCTIONS {{{
" Upload selection to termbin
function! Termbin() range
  echo system('echo '.shellescape(join(getline(a:firstline, a:lastline), "\\n")).'| sed s/\\\\\\\\\$// | nc termbin.com 9999 | pbcopy')
  echo "URL copied to clipboard"
endfunction

" Open terminal at current file's directory
command! -nargs=0 TermSplitH
  \ call TermSplitH()

function! TermSplitH()
    let cwd = expand('%:p:h')
    silent :call system('tmux split-pane -v -l15 -c ' . cwd)
endfunction

command! -nargs=0 TermSplitV
  \ call TermSplitV()

function! TermSplitV()
    let cwd = expand('%:p:h')
    silent :call system('tmux split-pane -h -l80 -c ' . cwd)
endfunction

function! ToggleRelativeNumber()
    if &rnu
        set nornu
    else
        set rnu
    endif
endfunction
"}}}
"-- DIFFING {{{
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
"}}}
"-- FZF {{{
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

function! s:fzf_edit_colour()
  call fzf#run({
        \ 'source': split(globpath(&rtp, 'colors/*.vim'), "\n"),
        \ 'sink':   'e',
        \ 'options': '-m -x +s',
        \ 'down': '40%', })
  call matchadd('Conceal', '.*/', 100, 67)
  setlocal conceallevel=2
  setlocal concealcursor=nvic
endfunction

inoremap <expr> <c-x><c-k> fzf#complete('cat /usr/share/dict/words')

function! s:fzf_statusline()
  highlight fzf1 ctermfg=161 ctermbg=251
  highlight fzf2 ctermfg=23 ctermbg=251
  highlight fzf3 ctermfg=237 ctermbg=251
  setlocal statusline=%{getcwd()}
endfunction

autocmd! User FzfStatusLine call <SID>fzf_statusline()

function! s:fzf_neighbouring_files()
  let current_file =expand("%")
  let cwd = fnamemodify(current_file, ':p:h')
  let command = 'ag -g "" -f ' . cwd . ' --depth 0'
  call fzf#run({
        \ 'source': command,
        \ 'sink':   'e',
        \ 'options': '-m -x +s',
        \ 'window': 'enew', })
  :exe 'setlocal statusline=' . cwd
  silent! call matchdelete(66)
  call matchadd('Conceal', '.*/', 100, 66)
  setlocal conceallevel=2
  setlocal concealcursor=nvic
endfunction

"}}}
"-- COMMANDS {{{

" Find argument in rtp
" example: Config haskell
command! -nargs=1 Config
  \ call fzf#run({
  \ 'source': reverse(split(globpath(&rtp, "*/" . <q-args> . "*"), "\n")),
  \ 'sink': 'e',
  \ 'down': '20%'})

command! -bang -nargs=* Ag
  \ call fzf#vim#ag(<q-args>,
  \                 <bang>0 ? fzf#vim#with_preview('up:60%')
  \                         : fzf#vim#with_preview('right:50%:hidden', '?'),
  \                 <bang>0)

command! ColEd
  \ call s:fzf_edit_colour()

command! -bang Colors
  \ call fzf#vim#colors({'left': '15%', 'options': '--reverse --margin 30%,0'}, <bang>0)

command! -bang -nargs=? -complete=dir Files
  \ call fzf#vim#files(<q-args>, fzf#vim#with_preview(), <bang>0)

" most recently used files
command! MRU call fzf#run({
  \  'source':  v:oldfiles,
  \  'sink':    'e',
  \  'options': '-m -x +s',
  \  'down':    '40%'})

command! FZFNeigh
  \ call s:fzf_neighbouring_files()

command! -nargs=0 MarkErrorLines
  \ call MarkErrorLines()
command! -nargs=0 ClearErrors
  \ call ClearErrors()

"}}}
"-- HIGHLIGHTS {{{
hi DiffAdd    ctermfg=NONE    ctermbg=237
hi DiffChange ctermfg=NONE    ctermbg=NONE
hi DiffDelete ctermfg=233     ctermbg=NONE
hi DiffText   ctermfg=yellow  ctermbg=NONE

hi Warning    ctermfg=NONE    ctermbg=240

sign define piet text=>> texthl=Error
sign define warning text=>> texthl=Warning
"}}}
"-- ERRORS {{{

"autocmd! QuickFixCmdPost [^l]* call MarkErrorLines()

function! ClearErrors()
  silent! call matchdelete(50)
  silent! call matchdelete(51)
  sign unplace *
endfunction

" Use the content of the qflist to mark erroneous lines
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

"}}}
"-- ABBREVIATIONS {{{
iabbrev \\|- ⊢
iabbrev \\|= ⊨
"}}}
"-- LaTeX {{{
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

autocmd FileType tex,bib setlocal spell!

set spellfile=~/.config/nvim/en.utf-8.add
set spelllang=en

"}}}
"-- SNIPPETS {{{
let g:UltiSnipsExpandTrigger       = "<tab>"
let g:UltiSnips#JumpForwards       = "<tab>"

"inoremap <c-k> <esc>:call UltiSnips#JumpBackwards()<cr>:startinsert<cr>

" If you want :UltiSnipsEdit to split your window.
let g:UltiSnipsEditSplit="vertical"
"}}}
"-- PLUGIN CONFIG {{{
let g:peekaboo_delay=500
let g:loaded_matchparen = 1

let g:gist_open_browser_after_post = 1
let g:gist_post_private = 1
let g:gist_update_on_write = 2

let g:buftabline_numbers = 2
let g:buftabline_indicators = 1
let g:buftabline_separators = 0
let g:gitgutter_diff_args = '-w'

let $FZF_DEFAULT_COMMAND = 'ag -g ""'
let g:fzf_buffers_jump = 1

let g:AutoPairsFlyMode = 1
au FileType hs let b:AutoPairs = {'(':')', '[':']', '{':'}','"':'"', '`':'`'}

let g:airline_extensions = ['tabline', 'branch', 'fugitive', 'hunks']
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#show_splits = 1
let g:airline#extensions#tabline#show_buffers = 1
let g:airline#extensions#hunks#enabled = 1
let g:airline#extensions#branch#enabled = 1
let g:airline#extensions#fugitive#enabled = 1

" GOYO
function! s:goyo_enter()
  silent !tmux set status off
  "silent !tmux list-panes -F '\#F' | grep -q Z || tmux resize-pane -Z
  set noshowmode
  set noshowcmd
  " ...
endfunction

function! s:goyo_leave()
  silent !tmux set status on
  "silent !tmux list-panes -F '\#F' | grep -q Z && tmux resize-pane -Z
  set showmode
  set showcmd
  " ...
endfunction

autocmd! User GoyoEnter nested call <SID>goyo_enter()
autocmd! User GoyoLeave nested call <SID>goyo_leave()

let g:tagbar_type_haskell = {
    \ 'ctagsbin'  : 'hasktags',
    \ 'ctagsargs' : '-x -c -o-',
    \ 'kinds'     : [
        \  'm:modules:0:1',
        \  'd:data: 0:1',
        \  'd_gadt: data gadt:0:1',
        \  't:type names:0:1',
        \  'nt:new types:0:1',
        \  'c:classes:0:1',
        \  'cons:constructors:1:1',
        \  'c_gadt:constructor gadt:1:1',
        \  'c_a:constructor accessors:1:1',
        \  'ft:function types:1:1',
        \  'fi:function implementations:0:1',
        \  'o:others:0:1'
    \ ],
    \ 'sro'        : '.',
    \ 'kind2scope' : {
        \ 'm' : 'module',
        \ 'c' : 'class',
        \ 'd' : 'data',
        \ 't' : 'type'
    \ },
    \ 'scope2kind' : {
        \ 'module' : 'm',
        \ 'class'  : 'c',
        \ 'data'   : 'd',
        \ 'type'   : 't'
    \ }
\ }
"}}}

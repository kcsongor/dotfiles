"-- PLUGIN MANAGER {{{
call plug#begin('~/.config/nvim/plugged')
"Plug 'SirVer/ultisnips'
"Plug 'brooth/far.vim'
"Plug 'ervandew/supertab'
"Plug 'ndmitchell/ghcid', { 'rtp': 'plugins/nvim' }
"Plug 'neomake/neomake'
"Plug 'sandeepcr529/Buffet.vim'
"Plug 'scrooloose/nerdtree'
Plug '/usr/local/opt/fzf'
Plug 'MattesGroeger/vim-bookmarks'
Plug 'Shougo/vimproc.vim'
Plug 'airblade/vim-gitgutter'
Plug 'ap/vim-buftabline'
Plug 'benmills/vimux'
Plug 'def-lkb/vimbufsync'
Plug 'godlygeek/tabular/'
Plug 'haya14busa/incsearch.vim'
Plug 'idris-hackers/idris-vim'
Plug 'itchyny/vim-cursorword' " underline word under cursor
Plug 'itchyny/vim-parenmatch'
Plug 'jiangmiao/auto-pairs'
Plug 'junegunn/fzf.vim'
Plug 'junegunn/goyo.vim'
Plug 'junegunn/gv.vim'
Plug 'junegunn/vim-peekaboo'
Plug 'justinmk/vim-sneak'
Plug 'jvoorhis/coq.vim'
Plug 'kcsongor/vim-buffer-manager'
Plug 'kcsongor/vim-hs'
Plug 'kcsongor/vim-itunes'
Plug 'kcsongor/vim-monochrome'
Plug 'kcsongor/vim-monochrome-light'
Plug 'kcsongor/vim-refactor'
Plug 'kcsongor/vim-thesis-writing'
Plug 'lervag/vimtex'
Plug 'majutsushi/tagbar'
Plug 'mattn/gist-vim'
Plug 'mattn/webapi-vim'
Plug 'mbbill/undotree'
Plug 'neovimhaskell/haskell-vim'
Plug 'qpkorr/vim-bufkill'
Plug 'the-lambda-church/coquille'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-obsession'
Plug 'tpope/vim-rhubarb'
Plug 'vim-scripts/LargeFile'
Plug 'wakatime/vim-wakatime'
Plug 'hecal3/vim-leader-guide'
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

if has("persistent_undo")
    set undodir=~/.undodir/
    set undofile
endif

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
" Define prefix dictionary
let g:lmap =  {}

" Mappings defined by leader-guide
let g:lmap.G = {
  \'name' : 'Git',
  \'s' : ['Gstatus', 'Git Status'],
  \'p' : ['Gpull',   'Git Pull'],
  \'u' : ['Gpush',   'Git Push'],
  \'c' : ['Gcommit', 'Git Commit'],
  \'w' : ['Gwrite',  'Git Write'],
  \'b' : ['Gblame',  'Git Blame'],
  \'d' : ['Gdiff',   'Git Diff'],
  \}

call leaderGuide#register_prefix_descriptions("<Space>", "g:lmap")
nnoremap <silent> <leader> :<c-u>LeaderGuide '<Space>'<CR>
vnoremap <silent> <leader> :<c-u>LeaderGuideVisual '<Space>'<CR>

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

"nnoremap <Leader>uq :call Unqualify()<cr>
"nnoremap <silent> <Leader>cc :silent! call ToggleConcealQualified()<cr>
map <C-h> :bprev<cr>
map <C-l> :bnext<cr>
nmap <Leader><Tab> :Buffers<cr>
nmap <Leader>cd :cd %:h<cr>
nmap <Leader>tr :call ToggleRelativeNumber()<CR>
nmap <Plug>Edit-haskell.vim :Config haskell<CR>
nmap <silent> <Leader>eh <Plug>Edit-haskell.vim<cr>
nnoremap <Plug>Edit-vimrc :e `=resolve(expand($MYVIMRC))`<CR>
nmap <leader>ev <Plug>Edit-vimrc
nmap <Leader>ma <Plug>BookmarkShowAll
nmap <Leader>mc <Plug>BookmarkClear
nmap <Leader>mg <Plug>BookmarkMoveToLine
nmap <Leader>mi <Plug>BookmarkAnnotate
nmap <Leader>mj <Plug>BookmarkNext
nmap <Leader>mjj <Plug>BookmarkMoveDown
nmap <Leader>mk <Plug>BookmarkPrev
nmap <Leader>mkk <Plug>BookmarkMoveUp
nmap <Leader>mm <Plug>BookmarkToggle
nmap <Leader>mx <Plug>BookmarkClearAll
nmap <silent> <Leader>sv :so $MYVIMRC<CR>
nmap [c <Plug>GitGutterPrevHunk
nmap ]c <Plug>GitGutterNextHunk
nnoremap <C-c><C-d> :Gdiff<cr>
nnoremap <Down> :resize +1<CR>
nnoremap <Leader>80 :vertical resize 80<CR>
nmap <Leader><Enter> <Plug>Show-all-commands-(FZF)
nnoremap <Plug>Show-all-commands-(FZF) :Commands<cr>
nnoremap <Leader><Leader> <C-^>
nnoremap <Leader><Tab> :Buffers<cr>
nmap <Leader>F <Plug>Repeat-last-search
nnoremap <Plug>Repeat-last-search :Ag! <C-R>o<cr>
nnoremap <Leader>a :Ag!<cr>
nnoremap <Leader>bc :BCommits<cr>
nnoremap <Leader>bl :BLines<cr>
nmap <Leader>f <Plug>Search-word
nnoremap <Plug>Search-word "oyaw :Ag! <C-R><C-W><cr>
nnoremap <Leader>gc :Commits<cr>
nnoremap <Leader>gf :call fzf#vim#ag("^module " . expand('<cWORD>'), {'options': '-1 -0'})<cr>
nmap <Leader>gg :GitGutterLineHighlightsToggle<cr>
nmap <Leader>gp <Plug>GitGutterPreviewHunk
nmap <Leader>gu <Plug>GitGutterUndoHunk
nnoremap <Leader>gt :call fzf#vim#tags(expand('<cword>'), {'options': '--exact --select-1 --exit-0'})<CR>
nnoremap <Leader>ne :FZFNeigh<cr>
nnoremap <Leader>t :Tabularize/
nnoremap <Leader>uf :UndotreeFocus<cr>
nnoremap <Leader>ut :UndotreeToggle<cr>
nnoremap <Left> :vertical resize +1<CR>
nnoremap <Right> :vertical resize -1<CR>
nnoremap <Up> :resize -1<CR>
nnoremap <expr> gb '`[' . strpart(getregtype(), 0, 1) . '`]'
nnoremap <leader>ic :echo CurrentTrack()<cr>
nnoremap <silent> --h "=HaskellModuleHeader()<CR>:0put =<cr>
nnoremap <silent> <C-c><C-c> :Hoogle <C-R><C-W><cr>
nnoremap <silent> <Leader>R :VimuxRunLastCommand<cr>
nmap <silent> <Leader>ec <Plug>Edit-cabal-file
nmap <silent> <Leader>ey <Plug>Edit-stack.yaml
nmap <Leader>gi <Plug>Jump-to-imports
nnoremap <silent> <Leader>ho :call Hoogle()<cr>
nnoremap <silent> <Leader>ii :call <SID>info(<SID>ident_under_cursor())<cr>
nnoremap <silent> <Leader>ik :call <SID>kind(<SID>paren_operator(<SID>ident_under_cursor()), 0)<cr>
nnoremap <silent> <Leader>ir :call <SID>kind("Rep " . <SID>paren_operator(<SID>ident_under_cursor()), 1)<cr>
nnoremap <silent> <Leader>it :call <SID>type(<SID>paren_operator(<SID>ident_under_cursor()))<cr>
nnoremap <silent> <Leader>lf :w<cr> :call LoadGhci()<cr>
nnoremap <silent> <Leader>li O<esc>80i-<esc>
nnoremap <silent> <Leader>mi :call <SID>insert_module_name()<cr>
nnoremap <silent> <Leader>r :call ReloadGHCI()<cr>
nmap <silent> <Leader>sb <Plug>Open-REPL
nmap <silent> <Leader>sc <Plug>View-core
nmap <silent> <Leader>tt <Plug>Type-under-cursor
nmap <silent> <Leader>wm <Plug>Identifier-information
noremap <C-p> :Files<cr>
noremap <Leader>ia :call ImportModule()<cr>
noremap <Leader>la :call AddLanguagePragma()<cr>
noremap <Leader>oa :call AddOption()<cr>
noremap <Leader>p :GFiles<cr>
nmap <Leader>si <Plug>Sort-imports
noremap <Plug>Sort-imports mz:Tabularize/as<CR>vip:sort<CR>`z
nmap <Leader>sl <Plug>Sort-language-pragmas
noremap <Plug>Sort-language-pragmas mzgg:Tabularize/#-}<CR>vip:!sort\|uniq<CR>`z
noremap <silent> <Leader>ioa :call AddOptionGHCI()<cr>
vnoremap <Leader>f "oy :Ag! <C-R>o<cr>
vnoremap <silent> <Leader>bb :'<,'>!brittany<cr>
vnoremap <silent> <Leader>sb :call Eval()<cr>
vnoremap <silent> t :call Termbin()<cr>


function! s:my_displayfunc()
    let g:leaderGuide#displayname =
    \ substitute(g:leaderGuide#displayname, '\c<cr>$', '', '')
    let g:leaderGuide#displayname = 
    \ substitute(g:leaderGuide#displayname, '^<Plug>', '', '')
    let g:leaderGuide#displayname = 
    \ substitute(g:leaderGuide#displayname, '-', ' ', 'g')
endfunction
let g:leaderGuide_displayfunc = [function("s:my_displayfunc")]

"---- buftabline {{{
nmap <leader>0 <Plug>Jump-to-buffer-10
nmap <Plug>Jump-to-buffer-10 <Plug>BufTabLine.Go(10)
nmap <leader>1 <Plug>Jump-to-buffer-1
nmap <Plug>Jump-to-buffer-1 <Plug>BufTabLine.Go(1)
nmap <leader>2 <Plug>Jump-to-buffer-2
nmap <Plug>Jump-to-buffer-2 <Plug>BufTabLine.Go(2)
nmap <leader>3 <Plug>Jump-to-buffer-3
nmap <Plug>Jump-to-buffer-3 <Plug>BufTabLine.Go(3)
nmap <leader>4 <Plug>Jump-to-buffer-4
nmap <Plug>Jump-to-buffer-4 <Plug>BufTabLine.Go(4)
nmap <leader>5 <Plug>Jump-to-buffer-5
nmap <Plug>Jump-to-buffer-5 <Plug>BufTabLine.Go(5)
nmap <leader>6 <Plug>Jump-to-buffer-6
nmap <Plug>Jump-to-buffer-6 <Plug>BufTabLine.Go(6)
nmap <leader>7 <Plug>Jump-to-buffer-7
nmap <Plug>Jump-to-buffer-7 <Plug>BufTabLine.Go(7)
nmap <leader>8 <Plug>Jump-to-buffer-8
nmap <Plug>Jump-to-buffer-8 <Plug>BufTabLine.Go(8)
nmap <leader>9 <Plug>Jump-to-buffer-9
nmap <Plug>Jump-to-buffer-9 <Plug>BufTabLine.Go(9)
"}}}
"---- find/replace {{{
nnoremap <silent> <leader>gw :silent! call FindSomeUsage(expand('<cword>'))<cr>
nnoremap <silent> <leader>ga :silent! call FindSomeUsage()<cr>
nmap <leader>% <Plug>Replace-word-in-file
nnoremap <Plug>Replace-word-in-file :%s/\<<C-r><C-w>\>//gI\|norm``<left><left><left><left><left><left><left><left><left><left>
nmap <leader>< <Plug>Replace-word-in-last-selection
nnoremap <Plug>Replace-word-in-last-selection :'<,'>s/\<<C-r><C-w>\>//gI\|norm``<left><left><left><left><left><left><left><left><left><left>
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
nmap <leader>x( <Plug>Delete-surrounding-(
nnoremap <Plug>Delete-surrounding-( mzdi(va(p`zh
nmap <leader>x[ <Plug>Delete-surrounding-[
nnoremap <Plug>Delete-surrounding-[ mzdi[va[p`zh
nmap <leader>x{ <Plug>Delete-surrounding-{
nnoremap <Plug>Delete-surrounding-{ mzdi{va{p`zh

vnoremap <leader>( <esc>a)<esc>gvo<esc>i(<esc>%
vnoremap <leader>[ <esc>a]<esc>gvo<esc>i[<esc>%
vnoremap <leader>{ <esc>a}<esc>gvo<esc>i{<esc>%

nmap <leader>( <Plug>Surround-word-with-(
nnoremap <Plug>Surround-word-with-( mzdiwi(<esc>pa)<esc>`zl
nmap <leader>[ <Plug>Surround-word-with-[
nnoremap <Plug>Surround-word-with-[ mzdiwi[<esc>pa]<esc>`zl
nmap <leader>{ <Plug>Surround-word-with-{
nnoremap <Plug>Surround-word-with-{ mzdiwi{<esc>pa}<esc>`zl
"}}}
"---- coq {{{
autocmd FileType coq nnoremap <buffer> <Leader>cn :CoqNext<cr>
autocmd FileType coq nnoremap <buffer> <Leader>cc :CoqToCursor<cr>
autocmd FileType coq nnoremap <buffer> <Leader>cu :CoqUndo<cr>
"}}}
"---- thesis {{{
autocmd FileType lhaskell nnoremap <buffer> <leader>gS :JumpToSection<cr>
autocmd FileType lhaskell nnoremap <buffer> <leader>gs :JumpToLabels section<cr>
autocmd FileType lhaskell nnoremap <buffer> <leader>gl :JumpToLabels 
autocmd FileType lhaskell nnoremap <buffer> <leader>lc :AddCitationFZF<cr>
"}}}
" Show highlight group
nnoremap <leader>hg :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<'
\ . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
\ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>

" Text objects
omap ic <Plug>GitGutterTextObjectInnerPending
omap ac <Plug>GitGutterTextObjectOuterPending
xmap ic <Plug>GitGutterTextObjectInnerVisual
xmap ac <Plug>GitGutterTextObjectOuterVisual

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

function! StartRefactor()
endfunction

function! StopRefactor()
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

let g:undotree_WindowLayout = 3
"}}}

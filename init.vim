""PLUGINS {{{
call plug#begin('~/.config/nvim/plugged')
Plug '/usr/local/opt/fzf'
Plug 'Shougo/vimproc.vim'
Plug 'airblade/vim-gitgutter'
Plug 'benmills/vimux'
Plug 'def-lkb/vimbufsync'
Plug 'godlygeek/tabular/'
Plug 'haya14busa/incsearch.vim'
Plug 'itchyny/vim-cursorword' " underline word under cursor
Plug 'itchyny/vim-parenmatch'
Plug 'jiangmiao/auto-pairs'
Plug 'junegunn/fzf.vim'
Plug 'junegunn/goyo.vim'
Plug 'junegunn/gv.vim'
Plug 'jvoorhis/coq.vim'
Plug 'kcsongor/vim-colour-manager'
Plug 'kcsongor/vim-hs'
Plug 'kcsongor/vim-itunes'
Plug 'kcsongor/vim-monochrome-light'
Plug 'kcsongor/vim-notes'
Plug 'kcsongor/vim-refactor'
Plug 'kcsongor/vim-tabbar'
Plug 'kcsongor/vim-thesis-writing'
Plug 'lervag/vimtex'
Plug 'majutsushi/tagbar'
Plug 'mattn/gist-vim'
Plug 'mattn/webapi-vim'
Plug 'mbbill/undotree'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rhubarb'
call plug#end()
""}}}
"COLOURS {{{
call colours#update_background()
call colours#lazy_colorscheme('monochrome-light')
"  }}}
"STATUS LINE {{{
function! init#status_new_file()
  if (filereadable(expand('%')))
    return ""
  else
    return " [·]"
  endif
endfunction

set statusline=%#Status1#\ %Y\ 
              \%#Status0#\ %{notes#statusline()}\ %=\ 
              \%{refactor#refactoring_mode()}\ %m\ \ 
              \%#Status1#%{init#status_new_file()}\ %f

"}}}
""GENERAL SETTINGS {{{
set exrc

if has("persistent_undo")
    set undodir=~/.undodir/
    set undofile
endif

let &shell = "zsh"
let mapleader = "\<Space>"
let maplocalleader = "\<Space>"
set autoindent
set backspace=indent,eol,start
set colorcolumn=80
set diffopt+=vertical " vertical split in diff
set expandtab
set foldmethod=marker
set hidden
set ignorecase
set lazyredraw
set nobackup
set nocursorline
set nonu
set nornu
set noswapfile
set nowrap
set scrolloff=0
set sessionoptions+=tabpages,globals
set smartcase
set splitbelow
set splitright
set sts=2
set sw=2
set virtualedit=block
set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.class,*.sjsir,*.o,*.hi

if (has('nvim'))
  set inccommand=nosplit
endif

filetype indent off

" Display tabs and trailing spaces
set listchars=tab:>~,nbsp:_,trail:.
set list

set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.o,*.hi
set iskeyword+=-

set tabline=%!tabbar#tabline()
"
""}}}
""AUTOCMD {{{
autocmd! User FzfStatusLine    call <SID>fzf_statusline()

augroup general
  autocmd!
  autocmd WinLeave     * set nornu
  autocmd WinEnter     * if &nu | set rnu | endif
  autocmd BufWritePost * call notes#update_notes_after_write()
augroup END

augroup notes
  autocmd!
  autocmd BufEnter *.notes nnoremap <buffer> <silent> <C-i> :call notes#jump_from_note()<cr>
  autocmd BufEnter *.notes nnoremap <buffer> <silent> <C-o> :tabclose<cr>
  autocmd BufEnter *.notes nnoremap <buffer> <silent> =     :tabclose<cr>
  autocmd BufEnter *.notes setlocal autoread
  autocmd BufEnter *.notes setlocal foldlevel=0
  autocmd BufWritePost *.notes call notes#refresh_notes()
augroup END

augroup haskell
  autocmd!
  autocmd FileType lhaskell call      init#lhaskell_mappings()
  autocmd FileType lhaskell,haskell   call init#haskell_mappings()
  autocmd FileType haskell            let b:AutoPairs = {'(':')', '[':']', '{':'}','"':'"', '`':'`'}
augroup END

augroup coq
  autocmd!
  autocmd FileType coq nnoremap <buffer> <leader>cn :CoqNext<cr>
  autocmd FileType coq nnoremap <buffer> <leader>cc :CoqToCursor<cr>
  autocmd FileType coq nnoremap <buffer> <leader>cu :CoqUndo<cr>
augroup END
""}}}
"MAPPINGS {{{
" Git
nnoremap <leader>gs   :Gstatus<cr>
nnoremap <leader>gp   :Gpull<cr>
nnoremap <leader>gu   :Gpush<cr>
nnoremap <leader>gc   :Gcommit<cr>
nnoremap <leader>gw   :Gwrite<cr>
nnoremap <leader>gb   :Gblame<cr>
nnoremap <leader>gd   :Gdiff<cr>
nnoremap <leader>ggh  :GitGutterLineHighlightsToggle<cr>
nnoremap <leader>ggs  :GitGutterSignsToggle<cr>
nnoremap <leader>ga   :GitGutterStageHunk<cr>
" Git log
nnoremap <leader>glb  :BCommits<cr>
nnoremap <leader>glc  :Commits<cr>
nnoremap <leader>gll  :GV<cr>
" Git hunks
nnoremap <leader>ghp  :GitGutterPreviewHunk<cr>
nnoremap <leader>ghu  :GitGutterUndoHunk<cr>
nmap     ]c           <Plug>GitGutterNextHunk
nmap     [c           <Plug>GitGutterPrevHunk
" Git refactoring
nnoremap <leader>%    :%s/\<<C-r><C-w>\>//gI\|norm``<left><left><left><left><left><left><left><left><left><left>
nnoremap <leader><    :'<,'>s/\<<C-r><C-w>\>//gI\|norm``<left><left><left><left><left><left><left><left><left><left>
nnoremap <leader>grf  :FindAllPrompt<cr>
nnoremap <leader>grr  :call refactor#replace_all_word(expand('<cword>'))<cr>
nnoremap <leader>grt  :ToggleRefactoring<cr>
nnoremap <leader>grw  :silent! call refactor#find_all_tab(expand('<cword>'))<cr>

" Jumps
nnoremap <leader>]  :call fzf#vim#tags(expand('<cword>'), {'options': '--exact --select-1 --exit-0'})<cr>
nnoremap <leader>ji :call haskell#jump_to_imports()<cr>
nnoremap <leader>jm :call fzf#vim#ag("^module " . expand('<cWORD>'), {'options': '-1 -0'})<cr>

" Note-taking
nnoremap <silent> = :call notes#jump_note()<cr>
nnoremap <silent> + :call notes#add_line_to_notes()<cr>
vnoremap <silent> + :call notes#add_line_to_notes()<cr>

" Terminal
if (has('nvim'))
  tnoremap <C-w><Bar> <C-\><C-n>:vsp term:///bin/zsh<cr>
  tnoremap <C-w>- <C-\><C-n>:sp term:///bin/zsh<cr>
  tnoremap <C-w> <C-\><C-n><C-w>
endif

nnoremap <C-w>-     :TermSplitH<cr>
nnoremap <C-w><Bar> :TermSplitV<cr>

nnoremap <C-h>  :tabprev<cr>
nnoremap <C-l>  :tabnext<cr>
nnoremap <C-w>, :call tabbar#rename_current_tab()<cr>

" For editing from a phone
nnoremap <leader>w  <C-w>
nnoremap <leader>tn :tabnew<cr>
nnoremap <leader>ll :tabn<cr>
nnoremap <leader>hh :tabp<cr>

" Finding things
nnoremap <C-p>           :Files<cr>
nnoremap <leader>F       :Ag! <C-R>o<cr>
nnoremap <leader>a       :Ag!<cr>
vnoremap <leader>f "oy   :Ag! <C-R>o<cr>

" Formatting
nnoremap <leader>t     :Tabularize/

" Buffers
nnoremap <leader><Tab> :Buffers<cr>
nnoremap <leader>bl    :BLines<cr>
nnoremap <leader>cd    :cd %:h<cr>
nnoremap <leader>f     :Ag! <C-R><C-W><cr>
nnoremap <leader>ne    :FZFNeigh<cr>
nnoremap <leader>p     :GFiles<cr>
nnoremap <leader>tr    :call init#toggle_relative_number()<CR>

" Undo
nnoremap <leader>uf    :UndotreeFocus<cr>
nnoremap <leader>ut    :UndotreeToggle<cr>

" Misc
nnoremap <leader>li      O<esc>80i-<esc>
nnoremap <leader>80      :vertical resize 80<CR>
nnoremap <expr> gb       '`[' . strpart(getregtype(), 0, 1) . '`]'
nnoremap <leader>ic      :echo CurrentTrack()<cr>
nnoremap <leader>R       :VimuxRunLastCommand<cr>
vnoremap t               :call init#termbin()<cr>

" vimrc
nnoremap <leader>ev :e `=resolve(expand($MYVIMRC))`<CR>
nnoremap <leader>sv :so $MYVIMRC<CR>

" When in diff mode, jump between hunks, otherwise location list items
nnoremap <expr> <C-j> &diff ? ']c' : ':lnext<cr>'
nnoremap <expr> <C-k> &diff ? '[c' : ':lprev<cr>'

function! init#haskell_mappings()
  nnoremap <buffer> --h "=HaskellModuleHeader()<CR>:0put =<cr>
  nnoremap <buffer> <C-c><C-c>   :Hoogle <C-R><C-W><cr>
  nmap     <buffer> <leader>ec   <Plug>Edit-cabal-file
  nnoremap <buffer> <leader>eh   :Config haskell<CR>
  nmap     <buffer> <leader>ey   <Plug>Edit-stack.yaml
  nnoremap <buffer> <leader>ho   :call Hoogle()<cr>
  nnoremap <buffer> <leader>ia   :call haskell#import_module()<cr>
  nmap     <buffer> <leader>ii   <Plug>:i-cword
  nmap     <buffer> <leader>ik   <Plug>:k-cword
  nnoremap <buffer> <leader>ioa  :call AddOptionGHCI()<cr>
  nmap     <buffer> <leader>ir   <Plug>:rep-cword
  nmap     <buffer> <leader>it   <Plug>:t-cword
  nnoremap <buffer> <leader>la   :call haskell#add_language_pragma()<cr>
  nnoremap <buffer> <leader>lf   :w<cr> :call LoadGhci()<cr>
  nnoremap <buffer> <leader>mi   :call <SID>insert_module_name()<cr>
  nnoremap <buffer> <leader>oa   :call haskell#add_compiler_flag()<cr>
  nnoremap <buffer> <leader>r    :call ReloadGHCI()<cr>
  nmap     <buffer> <leader>sb   <Plug>Open-REPL
  nmap     <buffer> <leader>sc   <Plug>View-core
  nnoremap <buffer> <leader>si   :Tabularize/as<CR>vip:sort<CR>`z
  nnoremap <buffer> <leader>sl   :Tabularize/#-}<CR>vip:!sort\|uniq<CR>`z
  nmap     <buffer> <leader>tt   <Plug>Type-under-cursor
  nmap     <buffer> <leader>wm   <Plug>Identifier-information

  " Working on GHC
  nmap     <buffer> <leader>jN <Plug>Find-note-under-cursor
  nmap     <buffer> <leader>jn <Plug>Find-notes
endfunction

function! init#lhaskell_mappings()
  nnoremap <buffer> <leader>jS :JumpToSection<cr>
  nnoremap <buffer> <leader>jc :AddCitationFZF<cr>
  nnoremap <buffer> <leader>jl :JumpToLabels 
  nnoremap <buffer> <leader>js :JumpToLabels section<cr>
endfunction

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

"--text manipulation {{{
nmap <leader>x( <Plug>Delete-surrounding-(
nmap     <Plug>Delete-surrounding-( mzdi(va(p`zh
nmap <leader>x[ <Plug>Delete-surrounding-[
nmap     <Plug>Delete-surrounding-[ mzdi[va[p`zh
nmap <leader>x{ <Plug>Delete-surrounding-{
nmap     <Plug>Delete-surrounding-{ mzdi{va{p`zh

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
"iPad MAPPINGS {{{


"}}}
"MISC FUNCTIONS {{{
" Upload selection to termbin
function! init#termbin() range
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

function! init#toggle_relative_number()
    if &rnu
        set nornu
    else
        set rnu
    endif
endfunction

"}}}
"DIFFING {{{
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
"FZF {{{
let g:fzf_colors =
\ { 'fg':      ['fg', 'Comment'],
  \ 'bg':      ['bg', 'Normal'],
  \ 'hl':      ['fg', 'String'],
  \ 'fg+':     ['fg', 'Normal', 'String', 'Normal'],
  \ 'bg+':     ['bg', 'Comment', 'CursorColumn'],
  \ 'hl+':     ['fg', 'Statement'],
  \ 'info':    ['fg', 'PreProc'],
  \ 'prompt':  ['fg', 'Conditional'],
  \ 'pointer': ['fg', 'Exception'],
  \ 'marker':  ['fg', 'Keyword'],
  \ 'spinner': ['fg', 'Label'],
  \ 'header':  ['fg', 'Comment'] }

let g:fzf_history_dir = '~/.local/share/fzf-history'

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


function! s:fzf_neighbouring_files()
  let current_file =expand("%")
  let cwd = fnamemodify(current_file, ':p:h')
  let command = 'ag -g "" -f ' . cwd . ' --depth 0'
  call fzf#run({
        \ 'source': command,
        \ 'sink':   'e',
        \ 'options': '-m -x +s',
        \ 'down': '40%'})
  :exe 'setlocal statusline=' . cwd
  silent! call matchdelete(66)
  call matchadd('Conceal', '.*/', 100, 66)
  setlocal conceallevel=2
  setlocal concealcursor=nvic
endfunction

"}}}
"COMMANDS {{{

" Find argument in rtp
" example: Config haskell
command! -nargs=1 Config
  \ call fzf#run({
  \ 'source': reverse(split(globpath(&rtp, "*/" . <q-args> . "*"), "\n")),
  \ 'sink': 'e',
  \ 'down': '20%',
  \ 'options' : '-1 -0'})

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
  \ call init#mark_error_lines()

command! -nargs=0 ClearErrors
  \ call init#clear_errors()

"}}}
"ERRORS {{{

function! init#clear_errors()
  silent! call matchdelete(50)
  silent! call matchdelete(51)
  sign unplace 2
endfunction

sign define piet text=>> texthl=Error
" Use the content of the qflist to mark erroneous lines
function! init#mark_error_lines()
  call init#clear_errors()
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

""}}}
""PLUGIN CONFIG {{{
let g:loaded_matchparen = 1

let g:gist_open_browser_after_post = 1
let g:gist_post_private = 1
let g:gist_update_on_write = 2

let g:gitgutter_diff_args = '-w'

let g:gitgutter_sign_added = '·'
let g:gitgutter_sign_modified = '·'
let g:gitgutter_sign_removed = '·'

let $FZF_DEFAULT_COMMAND = 'ag -g ""'
let g:fzf_buffers_jump = 1

let g:AutoPairsFlyMode = 0

let g:goyo_width="120"
let g:goyo_height="100%"
let g:goyo_linenr=1

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

set secure

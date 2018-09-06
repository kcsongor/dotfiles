"PLUGINS {{{1
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
Plug 'jreybert/vimagit'
Plug 'junegunn/fzf.vim'
Plug 'junegunn/goyo.vim'
Plug 'junegunn/gv.vim'
Plug 'jvoorhis/coq.vim'
Plug 'kcsongor/vim-colour-manager'
Plug 'kcsongor/vim-hs'
Plug 'kcsongor/vim-itunes'
Plug 'kcsongor/vim-monochrome'
Plug 'kcsongor/vim-notes'
Plug 'kcsongor/vim-refactor'
Plug 'kcsongor/vim-tabbar'
Plug 'kcsongor/vim-thesis-writing'
Plug 'lervag/vimtex'
Plug 'majutsushi/tagbar'
Plug 'mattn/gist-vim'
Plug 'mattn/webapi-vim'
Plug 'mbbill/undotree'
Plug 'radenling/vim-dispatch-neovim'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rhubarb'
call plug#end()
"PRESENTATIONS {{{1

if (!exists('g:presentation_mode'))
  let g:presentation_mode = 0
endif

function! init#check_presentation_mode()
  if (g:presentation_mode)
    set foldlevel=0
    set laststatus=0
    setlocal conceallevel=3
    setlocal concealcursor=nvic
    silent! call matchadd('Conceal', '{{{[0-9]\?', 100, 68)
    silent! call matchadd('Conceal', '_p[0-9]', 100, 69)
    set colorcolumn=0
  else
    set foldlevel=3
    set laststatus=2
    setlocal conceallevel=0
    silent! call matchdelete(68)
    silent! call matchdelete(69)
    set colorcolumn=80
  endif
endfunction

function! init#toggle_presentation_mode()
  let g:presentation_mode = abs(g:presentation_mode - 1)
  call init#check_presentation_mode()
endfunction

"COLOURS {{{1
call colours#update_background()
call colours#lazy_colorscheme('monochrome')
"STATUS LINE {{{1
function! init#status_new_file()
  if (filereadable(expand('%')))
    return ""
  else
    return " [·] "
  endif
endfunction

set statusline =
set statusline+=\ %n\ 
set statusline+=%#Status1#
set statusline+=\ %f:%l:%c\ \|
set statusline+=%#Status1#
set statusline+=%=
set statusline+=%{refactor#refactoring_mode()}
set statusline+=%m
set statusline+=%{init#status_new_file()}
set statusline+=%#Status0#
"set statusline+=\ %{fugitive#head()}\ %p%%\ 

"GENERAL SETTINGS {{{1
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
set noequalalways
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
set nowrapscan
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

" Tabline
set tabline=%!tabbar#tabline()

function! init#empty_foldtext()
  return ""
endfunction

" Fold text
set foldtext=init#fold_test()
function! init#fold_test()
  let line = getline(v:foldstart)
  let sub = substitute(line, '/\*\|\*/\|{{{\d\=', '', 'g')
  return repeat('--', v:foldlevel) . sub
endfunction

"AUTOCMD {{{1
autocmd! User FzfStatusLine    call <SID>fzf_statusline()

augroup general
  autocmd!
  autocmd WinLeave     * if &rnu | let w:rnu=1 | set nornu | else | let w:rnu=0 | endif
  autocmd WinEnter     * silent! if w:rnu | set rnu | endif
  autocmd BufWritePost * call notes#update_notes_after_write()
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
"MAPPINGS {{{1
" Git {{{2
nnoremap <leader>gs   :Gstatus<cr>
nnoremap <leader>gp   :Gpull<cr>
nnoremap <leader>gu   :Gpush<cr>
nnoremap <leader>gc   :Gcommit<cr>
nnoremap <leader>gw   :Gwrite<cr>
nnoremap <leader>gb   :Gblame<cr>
nnoremap <leader>gd   :Gdiff<cr>
nnoremap <leader>ga   :GitGutterStageHunk<cr>
" Git log {{{3
nnoremap <leader>glb  :BCommits<cr>
nnoremap <leader>glc  :Commits<cr>
nnoremap <leader>gll  :GV<cr>
" Git hunks {{{3
nnoremap <leader>ghp  :GitGutterPreviewHunk<cr>
nnoremap <leader>ghu  :GitGutterUndoHunk<cr>
nmap     ]c           <Plug>GitGutterNextHunk
nmap     [c           <Plug>GitGutterPrevHunk
" Git refactoring {{{3
nnoremap <leader>%    :%s/\<<C-r><C-w>\>//gI\|norm``<left><left><left><left><left><left><left><left><left><left>
nnoremap <leader><    :'<,'>s/\<<C-r><C-w>\>//gI\|norm``<left><left><left><left><left><left><left><left><left><left>
nnoremap <leader>grf  :FindAllPrompt<cr>
nnoremap <leader>grr  :call refactor#replace_all_word(expand('<cword>'))<cr>
nnoremap <leader>grt  :ToggleRefactoring<cr>
nnoremap <leader>grw  :silent! call refactor#find_all_tab(expand('<cword>'))<cr>

" Jumps {{{2
nnoremap <leader>]  :call fzf#vim#tags(expand('<cword>'), {'options': '--exact --select-1 --exit-0'})<cr>
nnoremap <leader>ji :call Haskell_jump_to_imports()<cr>
nnoremap <leader>jm :call fzf#vim#ag("^module " . expand('<cWORD>'), {'options': '-1 -0'})<cr>

" Note-taking {{{2
nnoremap <silent> = :call notes#jump_note()<cr>
nnoremap <silent> + :call notes#add_lines_to_notes()<cr>
vnoremap <silent> + :call notes#add_lines_to_notes()<cr>

" Terminal {{{2
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

" Tmux interaction {{{2

nnoremap <leader>> :VimuxPromptCommand<cr>
nnoremap <leader>vv :VimuxRunLastCommand<cr>
let g:VimuxPromptString="> "
let g:VimuxResetSequence="q C-u C-l"

" For editing from a phone {{{2
nnoremap <leader>w  <C-w>
nnoremap <leader>ll :tabn<cr>
nnoremap <leader>hh :tabp<cr>

" Finding things {{{2
nnoremap <C-p>           :Files<cr>
nnoremap <leader>F       :Ag! <C-R>o<cr>
nnoremap <leader>a       :Ag!<cr>
vnoremap <leader>f "oy   :Ag! <C-R>o<cr>

" Formatting {{{2
nnoremap <leader>t     :Tabularize/

" Presentation {{{2
nnoremap <leader>tp    :call init#toggle_presentation_mode()<cr>
" NOTE: set foldlevel to 0
nnoremap <Down> zjzxjzt[zzt
nnoremap <Up> zkzxkzt[zzt

" Buffers {{{2
nnoremap <leader><Tab> :Buffers<cr>
nnoremap <leader>bl    :BLines<cr>
nnoremap <leader>cd    :cd %:h<cr>
nnoremap <leader>lcd   :lcd %:h<cr>
nnoremap <leader>f     :Ag! <C-R><C-W><cr>
nnoremap <leader>ne    :FZFNeigh<cr>
nnoremap <leader>p     :GFiles<cr>

" Toggling things {{{2
nnoremap <leader>tr  :if &rnu \| set nornu \| else \| set rnu \| :endif<cr>
nnoremap <leader>tn  :if &nu \| set nonu \| se nornu \| else \| set nu \| :endif<cr>
nnoremap <leader>tg  :GitGutterLineHighlightsToggle<cr>
nnoremap <leader>ts  :GitGutterSignsToggle<cr>

" Undo {{{2
nnoremap <leader>uf    :UndotreeFocus<cr>
nnoremap <leader>ut    :UndotreeToggle<cr>

" Misc {{{2
nnoremap <leader>li      O<esc>80i-<esc>
nnoremap <leader>80      :vertical resize 80<CR>
nnoremap <expr> gb       '`[' . strpart(getregtype(), 0, 1) . '`]'
nnoremap <leader>ic      :echo CurrentTrack()<cr>
nnoremap <leader>R       :VimuxRunLastCommand<cr>
vnoremap t               :call init#termbin()<cr>

" vimrc {{{2
nnoremap <leader>ev :e `=resolve(expand($MYVIMRC))`<CR>
nnoremap <leader>sv :so $MYVIMRC<CR>

" When in diff mode, jump between hunks, otherwise location list items
nnoremap <expr> <C-j> &diff ? ']c' : ':lnext<cr>'
nnoremap <expr> <C-k> &diff ? '[c' : ':lprev<cr>'

nnoremap <expr> <C-a> &diff ? ':GitGutterStageHunk<cr>' : '<C-a>'

function! init#haskell_mappings()
  nnoremap <buffer> --h "=HaskellModuleHeader()<CR>:0put =<cr>
  nnoremap <buffer> <C-c><C-c>   :Hoogle <C-R><C-W><cr>
  nmap     <buffer> <leader>ec   <Plug>Edit-cabal-file
  nnoremap <buffer> <leader>eh   :Config haskell<CR>
  nmap     <buffer> <leader>ey   <Plug>Edit-stack.yaml
  nnoremap <buffer> <leader>ho   :call Hoogle()<cr>
  nnoremap <buffer> <leader>ia   :call Haskell_import_module()<cr>
  nmap     <buffer> <leader>ii   <Plug>:i-cword
  nmap     <buffer> <leader>ik   <Plug>:k-cword
  nnoremap <buffer> <leader>ioa  :call AddOptionGHCI()<cr>
  nmap     <buffer> <leader>ir   <Plug>:rep-cword
  nmap     <buffer> <leader>it   <Plug>:t-cword
  nnoremap <buffer> <leader>la   :call Haskell_add_language_pragma()<cr>
  nnoremap <buffer> <leader>lf   :w<cr> :call LoadGhci()<cr>
  nnoremap <buffer> <leader>mi   :call <SID>insert_module_name()<cr>
  nnoremap <buffer> <leader>oa   :call Haskell_add_compiler_flag()<cr>
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

" TEXT MANIPULATION {{{2
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
" Show highlight group {{{3
nnoremap <leader>hg :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<'
\ . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
\ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>

" Text objects {{{3
omap ic <Plug>GitGutterTextObjectInnerPending
omap ac <Plug>GitGutterTextObjectOuterPending
xmap ic <Plug>GitGutterTextObjectInnerVisual
xmap ac <Plug>GitGutterTextObjectOuterVisual

"MISC FUNCTIONS {{{1
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
    if (s:is_tmux())
      silent :call system('tmux split-pane -v -l15 -c ' . cwd)
    else
      exe "split term://".cwd."//zsh" | :startinsert
    endif
endfunction

command! -nargs=0 TermSplitV
  \ call TermSplitV()

function! TermSplitV()
    let cwd = expand('%:p:h')
    if (s:is_tmux())
      silent :call system('tmux split-pane -h -l80 -c ' . cwd)
    else
      exe "vsplit term://".cwd."//zsh" | :startinsert
    endif
endfunction

function! DeleteHiddenBuffers()
    let tpbl=[]
    call map(range(1, tabpagenr('$')), 'extend(tpbl, tabpagebuflist(v:val))')
    for buf in filter(range(1, bufnr('$')), 'bufexists(v:val) && index(tpbl, v:val)==-1')
        silent execute 'bwipeout' buf
    endfor
endfunction

function! s:is_tmux()
  return len(systemlist("echo $TMUX")) > 0
endfunction

"DIFFING {{{1
"set diffopt+=iwhite
set diffexpr=DiffW()

function! DiffW()
  let opt = ""
   if &diffopt =~ "icase"
     let opt = opt . "-i "
   endif
"   if &diffopt =~ "iwhite"
"     let opt = opt . "-w " " swapped vim's -b with -w
"   endif
   silent execute "!diff -a --binary " . opt .
     \ v:fname_in . " " . v:fname_new .  " > " . v:fname_out
endfunction

"FZF {{{1
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

"COMMANDS {{{1

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

"ERRORS {{{1

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

"PLUGIN CONFIG {{{1
let g:loaded_matchparen = 1

let g:gist_open_browser_after_post = 1
let g:gist_post_private = 1
let g:gist_update_on_write = 2

"let g:gitgutter_diff_args = '-w'

let g:gitgutter_sign_added = '│'
let g:gitgutter_sign_modified = '│'
let g:gitgutter_sign_removed = '│'

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

set secure

"}}}


" Peek definition of function below the cursor
nnoremap <leader>great :redir @a \| :echo expand('<cword>') \| :redir END \| :bel new \| :setlocal buftype=nofile \| :setlocal bufhidden=hide \| :norm "apddg]1<CR>

nnoremap <leader>pw :call PreviewWord()<cr>
func! PreviewWord()
  if &previewwindow " don't do this in the preview window
    return
  endif
  let w = expand("<cword>") " get the word under cursor
  if w =~ '\a' " if the word contains a letter

    " Delete any existing highlight before showing another tag
    silent! wincmd P " jump to preview window
    if &previewwindow " if we really get there...
      match none " delete existing highlight
      wincmd p " back to old window
    endif

    " Try displaying a matching tag for the word under the cursor
    try
       exe "ptag " . w
    catch
      return
    endtry

    silent! wincmd P " jump to preview window
    if &previewwindow " if we really get there...
      if has("folding")
        silent! .foldopen " don't want a closed fold
      endif
      call search("$", "b") " to end of previous line
      let w = substitute(w, '\\', '\\\\', "")
      call search('\<\V' . w . '\>') " position cursor on match
      " Add a match highlight to the word at this position
      hi previewWord term=bold ctermbg=green guibg=green
      exe 'match previewWord "\%' . line(".") . 'l\%' . col(".") . 'c\k*"'
      wincmd p " back to old window
    endif
  endif
endfun

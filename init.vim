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
Plug 'kcsongor/vim-hs'
Plug 'kcsongor/vim-monochrome'
Plug 'kcsongor/vim-monochrome-light'
Plug 'lervag/vimtex'
Plug 'mattn/gist-vim'
Plug 'mattn/webapi-vim'
Plug 'ndmitchell/ghcid', { 'rtp': 'plugins/nvim' }
Plug 'neomake/neomake'
Plug 'neovimhaskell/haskell-vim'
Plug 'qpkorr/vim-bufkill'
Plug 'scrooloose/nerdtree'
Plug 'sjl/gundo.vim'
Plug 'the-lambda-church/coquille'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rhubarb'
Plug 'wakatime/vim-wakatime'
Plug 'jiangmiao/auto-pairs'
Plug 'francoiscabrol/ranger.vim'
Plug 'ervandew/supertab'
Plug 'tomlion/vim-solidity'
Plug 'justinmk/vim-sneak'
Plug 'benmills/vimux'
Plug 'beloglazov/vim-online-thesaurus'
"Plug 'vim-airline/vim-airline'
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

map <C-l> :bnext<cr>
map <C-h> :bprev<cr>
nnoremap <C-c><C-d> :Gdiff<cr>

" select last pasted text
nnoremap <expr> gb '`[' . strpart(getregtype(), 0, 1) . '`]'

"---- terminal mode {{{
if (has('nvim'))
  tnoremap <C-w><Bar> <C-\><C-n>:vsp term:///bin/zsh<cr>
  tnoremap <C-w>- <C-\><C-n>:sp term:///bin/zsh<cr>
  nnoremap <C-w><Bar> :vsp<cr>:terminal<cr>
  nnoremap <C-w>- :sp<cr>:terminal<cr>
  tnoremap <C-w> <C-\><C-n><C-w>
endif
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
nnoremap <leader>th :call TermHere()<cr>

" Cd to current file
nmap <Leader>cd :cd %:h<cr>

" Locate file in nerdtree
nnoremap <leader>nf :NERDTreeFind<cr>
" Show neighbouring files in fzf
nnoremap <leader>ne :FZFNeigh<cr>
" fzf
nnoremap <leader>a :Ag!<cr>
" fzf with current word
nnoremap <leader>f "oyw :Ag! <C-R><C-W><cr>
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
nnoremap <leader>grw :call ReplaceAllWord(expand('<cword>'))<cr>
nnoremap <leader>gra :call ReplaceAll()<cr>

nnoremap <leader>gS :JumpToSection<cr>
nnoremap <leader>gs :JumpToSectionAll<cr>

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
nnoremap <leader>x( di(va(p``
nnoremap <leader>x[ di[va[p``
nnoremap <leader>x{ di{va{p``
vnoremap <leader>( <esc>a)<esc>gvo<esc>i(<esc>%
"}}}
"---- coq {{{
autocmd FileType coq nnoremap <Leader>cn :CoqNext<cr>
autocmd FileType coq nnoremap <Leader>cc :CoqToCursor<cr>
autocmd FileType coq nnoremap <Leader>cu :CoqUndo<cr>
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
function! TermHere()
    let cwd = expand('%:p:h')
    silent :call system('tmux split-pane -v -l15 -c ' . cwd)
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
  call matchdelete(66)
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

command! Song
  \ call s:fzf_pick_song()

command! Artist
  \ call s:fzf_pick_artist()

command! Playlist
  \ call s:fzf_pick_playlist()

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

command! -nargs=1 -complete=tag FindAll
  \ silent! call FindSomeUsage(<q-args>)

command! -nargs=1 -complete=tag FindCwd
  \ silent! call FindSomeUsage(<q-args>, getcwd())

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
"-- REFACTORING {{{
function! FindSomeUsage(...)
  call matchdelete(66)
  let word = 0 < a:0 ? a:1 : inputdialog("Word to search for: ")
  hi FoundGroup ctermbg=blue ctermfg=white
  let in_dir = ""
  if (a:0 > 1)
    let in_dir = " -- ".a:2
  endif
  exe "Glgrep! -w " . shellescape(word) . in_dir
  ldo call matchadd('FoundGroup', '\<' . word . '\>', 100, 66)
endfunction

function! ReplaceAllWord(...)
  call matchdelete(66)
  let word = 0 < a:0 ? a:1 : inputdialog("Word to replace: ")
  let to = 1 < a:0 ? a:2 : inputdialog("Replace (" . word . ") with: ")
  let in_dir = ""
  if (a:0 > 2)
    let in_dir = " -- ".a:3
  endif
  exe "Glgrep! -w " . shellescape(word) . in_dir
  exe "ldo %s/\\<" . word . "\\>/" . to . "/gcI \| update"
endfunction

function! ReplaceAll(...)
  let str = 0 < a:0 ? a:1 : inputdialog("String to replace: ")
  let to = 1 < a:0 ? a:2 : inputdialog("Replace (" . str . ") with: ")
  let in_dir = ""
  if (a:0 > 2)
    let in_dir = " -- ".a:3
  endif
  exe "Glgrep! " . shellescape(str) . in_dir
  exe "ldo %s/" . str . "/" . to . "/gcI \| update"
endfunction

"}}}
"-- ERRORS {{{

autocmd! QuickFixCmdPost [^l]* call MarkErrorLines()

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
"}}}
"-- iTunes CONTROL {{{
" Uses the 'songs' binary
function! s:fzf_pick_song()
  let songs = split(system("songs list-all"), '\n')
  let selected = []
  call fzf#run({
        \ 'source': songs,
        \ 'sink*':   function('s:make_playlist'),
        \ 'options': '--reverse --multi',
        \})
  call matchadd('Conceal', '[0-9]\+: ', 100, 67)
  setlocal conceallevel=2
  setlocal concealcursor=nvic
endfunction

function! s:make_playlist(lines)
  let nums = []
  for line in a:lines
    :call add(nums, split(line, ":")[0])
  endfor
  :call jobstart("songs play-tracks " . join(nums, " "))
endfunction

function! s:fzf_pick_artist()
  let songs = split(system("songs list-artists"), '\n')
  let selected = []
  call fzf#run({
        \ 'source': songs,
        \ 'sink':   {i -> jobstart("songs play-artist " . shellescape(i))},
        \ 'options': '--reverse',
        \})
endfunction

function! s:fzf_pick_playlist()
  let songs = split(system("songs list-playlists"), '\n')
  let selected = []
  call fzf#run({
        \ 'source': songs,
        \ 'sink':   {i -> jobstart("songs play-playlist " . shellescape(i))},
        \ 'options': '--reverse',
        \})
endfunction

function! s:current_track()
  let queries = ['album of the current track',
               \ 'artist of the current track',
               \ 'name of the current track',
               \ 'the player position']
  let query = "osascript -s s -e 'tell application \"iTunes\" to return "
             \ . join(queries, ' & "----" & ') . "'"

  let song_info = split(split(system(query), "\n")[0][1:-1], "----")
  return { 'album'  : song_info[0],
         \ 'artist' : song_info[1],
         \ 'title'  : song_info[2],
         \ 'seconds': split(song_info[3], '\.')[0]
         \ }
endfunction

function! CurrentTrack()
  let track = s:current_track()
  return track.artist . " - " . track.title . " (" . track.album . ")"
endfunction!

nnoremap <leader>ic :echo CurrentTrack()<cr>

"}}}

"-- Scratch {{{
"}}}

"-- Thesis {{{

command! -nargs=0 IncludesRel
  \ call fzf#run({
  \ 'source': Includes(expand('%')),
  \ 'sink': 'e',
  \ 'options': '--reverse --header ":: Select include" --prompt "Includes> "',
  \ 'down': '20%'})

command! -nargs=0 Includes
  \ call fzf#run({
  \ 'source': Includes(expand('~/Dev/haskell/meng/report/src/Main.lhs')),
  \ 'sink': 'e',
  \ 'options': '--reverse --header ":: Select include" --prompt "Includes> "',
  \ 'down': '20%'})

command! -nargs=0 JumpToSection
  \ call JumpToSection(expand('%'))

command! -nargs=0 JumpToSectionAll
  \ call JumpToSectionAll()


" Rreturns included lhs files in order of inclusion
function! Includes(fname)
  let path = fnamemodify(a:fname, ':.:h')
  let lhs = split(globpath(path . '/**', '*.lhs'), '\n')
  let includes = filter(readfile(a:fname), 'v:val =~ "%include .*.lhs"')
  let ret = []
  for i in includes
    let incname = substitute(i, "%include ", "", "g")
    let incfiles = filter(copy(lhs), 'v:val =~ "' . incname . '"')
    if (len(incfiles) > 0)
      let ret = add(ret, incfiles[0])
      let trans = Includes(incfiles[0])
      let ret = extend(ret, trans)
    endif
  endfor
  return ret
endfunction

function! Sections(fname)
  let red = "\u001b[31m"
  let reset = "\u001b[0m"
  let green = "\u001b[32m"
  let bright_black = "\u001b[33m"
  let fcontents = map(readfile(a:fname), {key, val -> green.key.reset. ':' . val})
  let sections = filter(fcontents, 'v:val =~ "section{"')
  let sections = filter(sections, 'v:val !~ "%"')
  let titles = []
  for section in sections
    let ln = substitute(section, '\\\w\+{\([^}]\+\)}', "\\1", "")
    let ln = substitute(ln, '\\label{\([^}]\+\)}', bright_black."(\\1)".reset, "")
    let titles = add(titles, red.a:fname.reset.':'.ln)
  endfor
  return {'section_titles': titles, 'lines': sections}
endfunction

function! AllSections()
  let main = fnamemodify(expand('~/Dev/haskell/meng/report/src/Main.lhs'), ':.')
  let includes = extend([main], Includes(main))
  let sections = []
  for inc in includes
    let sections = extend(sections, Sections(inc).section_titles)
  endfor
  return sections
  " echo includes
endfunction

function! JumpToSectionAll()
  let ss = AllSections()
    "\ 'source': ss.section_titles,
  call fzf#run({
    \ 'source' : ss,
    \ 'sink' : function('s:jump_to_section_sink'),
    \ 'options': '--ansi --reverse --header ":: Select section" --prompt "Section> "',
    \ 'down': '20%'})
endfunction


function! JumpToSection(fname)
  let ss = Sections(a:fname)
    "\ 'source': ss.section_titles,
  call fzf#run({
    \ 'source' : ss.section_titles,
    \ 'sink' : function('s:jump_to_section_sink'),
    \ 'options': '--ansi --reverse --header ":: Select section" --prompt "Section> "',
    \ 'down': '20%'})
endfunction

function! s:jump_to_section_sink(line)
  let ms = split(a:line, ':')
  let file = ms[0]
  let lnum = ms[1]
  execute "edit +".(lnum+1)." ".file
endfunction

"}}}

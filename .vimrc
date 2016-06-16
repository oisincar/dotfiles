
"   ___  _     _       _              _                    
"  / _ \(_)___(_)_ __ ( )___   __   _(_)_ __ ___  _ __ ___ 
" | | | | / __| | '_ \|// __|  \ \ / / | '_ ` _ \| '__/ __|
" | |_| | \__ \ | | | | \__ \   \ V /| | | | | | | | | (__ 
"  \___/|_|___/_|_| |_| |___/  (_)_/ |_|_| |_| |_|_|  \___|
"

" Title font: big. Subtitle font: straight.
" http://patorjk.com/software/taag

"    __                                          
"   |__)|    _ . _  _   _  _  _|  |_|_  _ _  _ _ 
"   |   ||_|(_)|| )_)  (_|| )(_|  |_| )(-|||(-_) 
"           _/                                   
let $nvim_tui_enable_true_color=1 " nvim true colour
set t_Co=256

" --------- AUTOSETUP VIM FOLDER -------
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')

" -------- COMPLETION ----------
function! DoRemote(arg)
    UpdateRemotePlugins
endfunction

if has('nvim')
    Plug 'Shougo/deoplete.nvim', { 'do': function('DoRemote') }
    " Plug 'Shougo/neosnippet'
    " Plug 'Shougo/neosnippet-snippets'
endif


" -------- EDITING ----------
Plug 'Raimondi/delimitMate'
Plug 'godlygeek/tabular',       { 'for': 'markdown' } " Required for vim-markdown.
Plug 'plasticboy/vim-markdown', { 'for': 'markdown' }
Plug 'tpope/vim-commentary'

Plug 'sheerun/vim-polyglot' " Syntax highlighting for every language imaginable.

" ------- BUILDING ---------
Plug 'Shougo/vimproc.vim', { 'do': 'make' } " For command line operations from within vim.
Plug 'benekastah/neomake'   " Used for haskell typechecking, but I should 
                            " use it for other stuff so it goes here..

" -------- COLOUR ----------
Plug 'ewilazarus/preto'
Plug 'fxn/vim-monochrome'
Plug 'robertmeta/nofrils'
Plug 'morhetz/gruvbox' 

" ------- NAVIGATION --------
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
Plug 'wellle/targets.vim'

Plug 'Shougo/unite.vim'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }

" ---------- MISC -----------
Plug 'bling/vim-airline'
Plug 'tpope/vim-fugitive'

" -------- LANGUAGES ----------
" csharp
Plug 'omnisharp/omnisharp-vim', { 'for': 'csharp', 'rtp': 'vim', 'do': 'cd server; xbuild' }

" haskell
Plug 'eagletmt/ghcmod-vim',             { 'for': 'haskell' }
Plug 'eagletmt/neco-ghc',               { 'for': 'haskell' }
Plug 'Twinside/vim-hoogle',           	{ 'for': 'haskell' }
"Plug 'mpickering/hlint-refactor-vim', 	{ 'for': 'haskell' }
"Plug 'enomsg/vim-haskellConcealPlus', 	{ 'for': 'haskell' }

call plug#end()

" --------- COLOUR SCHEMES/ AIRLINE ---------
set background=dark    " setting dark mode
let g:gruvbox_contrast_dark='hard'
colorscheme gruvbox

let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts = 1 
let g:airline_theme='gruvbox' 
let g:airline#extensions#wordcount#enabled = 1
let g:airline#extensions#whitespace#enabled = 0

" --------------- DEOMPLETE -----------------
let g:deoplete#enable_at_startup = 1
" let g:deoplete#disable_auto_complete = 1

if !exists('g:deoplete#omni#input_patterns')
  let g:deoplete#omni#input_patterns = {}
endif

autocmd InsertLeave,CompleteDone * if pumvisible() == 0 | pclose | endif

" deoplete tab-complete
inoremap <silent><expr> <Tab> pumvisible() ? "\<C-n>" : deoplete#mappings#manual_complete()
" Space <Tab> for regular tab... Eh... This sucks.
inoremap <Leader><Tab> <Space><Space>

" ------------- DELIMITMATE --------------
" Need to look into these more, for now just nice opening of brackets.
let delimitMate_expand_cr=1 
let delimitMate_expand_space=1

" ---------------------------------------------------
" ------------------  LANGUAGES ---------------------
" ---------------------------------------------------

" --------- MARKDOWN -----------
" Disable automatic folding of markdown files.
let g:vim_markdown_folding_disabled = 1


" ---------- C-SHARP ----------
let g:OmniSharp_selector_ui = 'unite'

" --------- HASKELL -----------
let g:haskell_conceal_wide = 1
let g:haskell_conceal_enumerations = 1
"let hscoptions="𝐒𝐓𝐄𝐌xRtB𝔻wr"
let hscoptions="℘𝐒𝐓𝐄𝐌xErbl↱w-iRtBQZ𝔻A"

" Show types in completion suggestions
let g:necoghc_enable_detailed_browse = 1
let g:haskellmode_completion_ghc = 0
autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc

" Highlight everything we got.. Not sure this is doing anything with the new
" setup. (Vim poligot)
let g:haskell_enable_quantification = 1
let g:haskell_enable_recursivedo = 1
let g:haskell_enable_arrowsyntax = 1
let g:haskell_enable_pattern_synonyms = 1
let g:haskell_enable_typeroles = 1
let g:haskell_enable_static_pointers = 1

" Disable hlint-refactor-vim's default keybindings
let g:hlintRefactor#disableDefaultKeybindings = 1


"    __                                   
"   / _  _ _  _ _. _   _ _|_|_. _  _  _   
"   \__)(-| )(-| |(_  _)(-|_|_|| )(_)_).  
"                                 _/     

set spell spelllang=en_gb

set history=1000         " Can't have too much history!  
set timeoutlen=400

set noswapfile	         " Don't keep swapfiles.
set nobackup             " Use source control instead...
set undofile             " Keep an undo file (undo changes after closing).
set hls                  " Highlight search.
set ruler                " Show the cursor position all the time.
set showcmd              " Display incomplete commands in corner.
set number               " Show line number.
set relativenumber       " Relative line number.
set incsearch            " Real time search highlighting.

set ignorecase smartcase " Clever searching
set nolazyredraw         " Fix rendering of terminal (?)


" Highlight current line
set cursorline
set cmdheight=1
set switchbuf=vsplit,useopen
set showtabline=2

set splitbelow splitright " open new window splits to the right and bottom of current.

" Keep space around the cursor when scrolling/panning.
set sidescroll=1
set scrolloff=8       " 8 lines minimum to top/bottom.
set sidescrolloff=15  " 15 columns minimum to left/right.

" FUCK YOUR STUPID BELL STFU HOLY SHIT
set visualbell         

" Smarttab. Set tab width to 4 spaces, and convert tabs to spaces.
set smarttab 
set tabstop=4 shiftwidth=4 expandtab 


"   
"   |_/ _  |_ . _  _|. _  _  _ 
"   | \(-\/|_)|| )(_||| )(_)_) 
"        /               _/    

let mapleader="\<Space>"

" ------- FILE NAVIGATION -------
" Toggle Nerdtree.
noremap <leader>t :NERDTreeToggle<cr> 

" Close vim if only window left open in nerdtree.
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

noremap <leader>rc :vsp ~/.vimrc<CR>

" ------ SPLITS/BUFFERS ---------

" Jump to other open buffer.
nnoremap <leader><leader> <C-^>

" Use | or _ to open a vertical split or horizontal split.
nnoremap <expr><silent> <Bar> v:count == 0 ? "<C-W>v<C-W><Right>" : ":<C-U>normal! 0".v:count."<Bar><CR>"
nnoremap <expr><silent> _     v:count == 0 ? "<C-W>s<C-W><Down>"  : ":<C-U>normal! ".v:count."_<CR>"

" Equalise splits upon resizing vim.
autocmd VimResized * execute "normal! \<c-w>="

" Use tab and shift-tab to cycle through windows in normal and terminal mode.
" Disabled because it interferes with jump-list.
" nnoremap <Tab> <C-W>w
nnoremap <S-Tab> <C-W>w

" " Move around splits with <Alt-hjkl>.
" nnoremap <M-j> <C-W><C-J>
" nnoremap <M-k> <C-W><C-K>
" nnoremap <M-l> <C-W><C-L>
" nnoremap <M-h> <C-W><C-H>
" inoremap <M-j> <Esc><C-W><C-J>
" inoremap <M-k> <Esc><C-W><C-K>
" inoremap <M-l> <Esc><C-W><C-L>
" inoremap <M-h> <Esc><C-W><C-H>
" if has('nvim') " As above, exit terminal mode, and switch window.
"   tnoremap <M-j> <C-\><C-n><C-W><C-J>
"   tnoremap <M-k> <C-\><C-n><C-W><C-K>
"   tnoremap <M-l> <C-\><C-n><C-W><C-L>
"   tnoremap <M-h> <C-\><C-n><C-W><C-H>
" endif

" Move around splits with <leader>-hjkl.
nnoremap <leader>j <C-W><C-J>
nnoremap <leader>k <C-W><C-K>
nnoremap <leader>l <C-W><C-L>
nnoremap <leader>h <C-W><C-H>

if has('nvim') " Enter insert mode when changing focus to a terminal split.
  autocmd BufWinEnter,WinEnter term://* startinsert
endif

" Also move splits around with <leader>-HJKL.
" ('Pushes' split as far as possible in given direction.)
nnoremap <leader>J <C-W>J
nnoremap <leader>K <C-W>K
nnoremap <leader>L <C-W>L
nnoremap <leader>H <C-W>H

" -------- CODE NAVIGATION --------
" qj or jq to escape normal mode
inoremap qj <Esc>
inoremap jq <Esc>

" Use escape/ qj/ jq to exit terminal mode.
if has('nvim')
  tnoremap <Esc> <C-\><C-n>
  tnoremap qj <C-\><C-n>
  tnoremap jq <C-\><C-n>
endif

" Easier moving around wrapped lines
nmap j gj
nmap k gk

" -------------- FIXES ---------------
" Allows an undo of ctrl-u
inoremap <C-U> <C-G>u<C-U> 	

" Make Y work like it's supposed to. (copy to end of line, not whole line)
nnoremap Y y$ 

" ---------- COPYING/PASTING -----------
" Make a line above and below, then paste from buffer.
nnoremap <leader>sp o<CR><Esc>k<Esc>"+p

" Replace file with buffer. For proj Euler.
nnoremap <leader>dp ggvG"+p

" Copy entire file into register
nnoremap <leader>c :%y+<CR>

" ---------- UTILITY/MISC -----------
" Clears highlighting after search
nnoremap <silent><leader>o :nohl<CR>

" Swap ; and : 
nnoremap ; :
nnoremap : ;
vnoremap ; :
vnoremap : ;

" Toggle spell check, and allow for sp-tab auto complete in normal mode
nnoremap <silent><leader>s :set spell!<CR> 
noremap <silent><leader><Tab> 1z=

" Save file with sp-w.
nnoremap <leader>w :w<cr>

" Insert mode shows deterministic line no, normal mode shows relative.
autocmd InsertEnter * :set norelativenumber
autocmd InsertLeave * :set relativenumber

" Autosave all on focus lost (Terminal vim doesn't proc this but.. Eh)
autocmd FocusLost * :wa

" Mappings for college computers...
"else
"    noremap § $
"    noremap 1 & 
"    noremap 2 [
"    noremap 3 {
"    noremap 4 }
"    noremap 5 (
"    noremap 6 @
"    noremap 7 *
"    noremap 8 )
"    noremap 9 ^
"    noremap 0 ]
"    noremap [ !
"    noremap ] #

"    noremap ± ~
"    noremap ! % 
"    noremap @ 7
"    noremap # 5
"    noremap $ 3
"    noremap % 1
"    noremap ^ 9
"    noremap & 0
"    noremap * 2
"    noremap ( 4
"    noremap ) 6
"    noremap { 8
"    noremap } `
"
"    noremap ; '
"    noremap : "
"    noremap ' ;
"    noremap " :
" endif

" ---------- HASKELL OVERLORDS ------------
" (Show) || (Insert in line above) the type of the expression under cursor
nnoremap <silent> <leader>st :GhcModType<CR>
nnoremap <silent> <leader>sT :GhcModTypeInsert<CR>
" Clear highlighting
nnoremap <silent><leader>so :GhcModTypeClear<CR>

" GHC errors and warnings
let g:neomake_haskell_ghc_mod_args = '-g-Wall'
nnoremap <silent> <leader>sc :Neomake ghcmod<CR>

" Hoogle search (shows type/ matching functions). 
" Search for word under cursor, or prompt for input
nnoremap <leader>sh :Hoogle<CR> 
nnoremap <leader>sH :Hoogle              

" Hoogle info (Detailed info for specific func)
" Search for word under cursor, or prompt for input
nnoremap <leader>si :HoogleInfo<CR> 
nnoremap <leader>sI :HoogleInfo              

" GHC Lint checker (Reports unnessasary brackets, hidden variables etc.)
nnoremap <leader>sl :GhcModCheckAndLintAsync

" Close the Hoogle window
nnoremap <silent> <leader>sq :HoogleClose<CR> 


"   
"   |\ | _ _ |_  (_    _  _|_. _  _  _ |   _ _ . _  _ .|_ _ 
"   | \|(-(_||_  | |_|| )(_|_|(_)| )(_||  _)| )||_)|_)||__) 
"                                               |  |  

" Remember cursor's position in file.
augroup vimrcEx
  autocmd!
  autocmd FileType text setlocal textwidth=120
  autocmd BufReadPost *
    \ if line("'\"") > 1 && line("'\"") <= line("$") |
    \   execute "normal! g`\"" |
    \ endif
augroup END

" Convenient command to see the difference between the current buffer and the
" file it was loaded from, thus the changes you made.
if !exists(":DiffOrig")
  command DiffOrig vert new | set buftype=nofile | read ++edit # | 0d_ | diffthis
                 \ | wincmd p | diffthis
endif

" Pasting over selected area doesn't replace paste buffer.
function! RestoreRegister()
  let @" = s:restore_reg
  return ''
endfunction
function! s:Repl()
  let s:restore_reg = @"
  return "p@=RestoreRegister()\<cr>"
endfunction
vmap <silent> <expr> p <sid>Repl()

" Make current window more obvious by turning off/adjusting some features in non-current windows. Except command-t, for now.
let g:WincentColorColumnBlacklist = ['diff', 'undotree', 'nerdtree', 'qf']
let g:WincentCursorlineBlacklist = ['command-t']

" if exists('+colorcolumn')
"   autocmd BufEnter,FocusGained,VimEnter,WinEnter * if Should_colorcolumn() | let &l:colorcolumn='+' . join(range(0, 254), ',+') | endif
"   autocmd FocusLost,WinLeave * if Should_colorcolumn() | let &l:colorcolumn=join(range(1, 255), ',') | endif
" endif

" function! Should_colorcolumn() abort
"   return index(g:WincentColorColumnBlacklist, &filetype) == -1
" endfunction

" function! Should_cursorline() abort
"   return index(g:WincentCursorlineBlacklist, &filetype) == -1
" endfunction




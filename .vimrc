
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


if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')

<<<<<<< HEAD
" -------- COMPLETION ----------
if has('nvim')
	Plug 'valloric/youcompleteme', { 'do': './install.py --all' }
endif
=======
" Completion
"if liteVersion:
if has('nvim')
    Plug 'valloric/youcompleteme'
endif
"Plug 'scrooloose/syntastic' 
>>>>>>> c8badadb943606a3700428bf03e7831658c49c85

" -------- EDITING ----------
Plug 'Raimondi/delimitMate'
Plug 'godlygeek/tabular', 		{ 'for': 'markdown' }" Required for vim-markdown.
Plug 'plasticboy/vim-markdown', { 'for': 'markdown' }
Plug 'tpope/vim-commentary'
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

" ---------- MISC -----------
Plug 'bling/vim-airline'
Plug 'tpope/vim-fugitive'

" -------- HASKELL ----------
Plug 'Shougo/vimproc.vim', 				{ 'do': 'make' }
Plug 'neovimhaskell/haskell-vim',   	{ 'for': 'haskell' }
Plug 'eagletmt/ghcmod-vim',           	{ 'for': 'haskell' }
Plug 'eagletmt/neco-ghc',             	{ 'for': 'haskell' }
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
<<<<<<< HEAD

" ------------- YOUCOMPLETEME ---------------
if has('nvim')
    let g:Show_diagnostics_ui = 1 "default 1
    let g:YcmShowDetailedDiagnostic = 1
    let g:ycm_enable_diagnostic_signs = 1

    " (Haskell)
    " Use symbols from file if (<4) chars, otherwise search full list of
    " functions. It's pretty hacky, but there doesn't seem to be a way 
    " to do combine symbols from the buffer and from haskell and necoghc.
    let g:ycm_semantic_triggers = {'haskell' : ['.']}
    let g:ycm_semantic_triggers.haskell = ['re!(?=[a-zA-Z_]{4})'] 
=======
" only enable word count for selected filetypes.
"let g:airline#extensions#wordcount#filetypes = ...


" YouCompleteMe options 
if has('nvim')
    " Uncomment this to re-enable syntastic.
    "let g:ycm_register_as_syntastic_checker = 1 "default 1
    let g:Show_diagnostics_ui = 1 "default 1
    
    let g:ycm_enable_diagnostic_signs = 1
    "let g:ycm_enable_diagnostic_highlighting = 0
    "let g:ycm_always_populate_location_list = 1 "default 0
    "let g:ycm_open_loclist_on_ycm_diags = 1 "default 1
    "
    "let g:ycm_complete_in_strings = 1 "default 1
    "let g:ycm_collect_identifiers_from_tags_files = 0 "default 0
    "let g:ycm_path_to_python_interpreter = '' "default ''
    "
    "let g:ycm_server_use_vim_stdout = 0 "default 0 (logging to console)
    "let g:ycm_server_log_level = 'info' "default info
    "
    "let g:ycm_global_ycm_extra_conf = '~/.ycm_extra_conf.py'  "where to search for .ycm_extra_conf.py if not found
    "let g:ycm_confirm_extra_conf = 0
    "
    "let g:ycm_goto_buffer_command = 'same-buffer' "[ 'same-buffer', 'horizontal-split', 'vertical-split', 'new-tab' ]
    "let g:ycm_filetype_whitelist = { '*': 1 }
    "let g:ycm_key_invoke_completion = '<C-Space>'
    
    let g:ycm_semantic_triggers = {'haskell' : ['.']}
>>>>>>> c8badadb943606a3700428bf03e7831658c49c85
endif

" ------------ DELIMITMATE -------------
" Need to look into these more, for now just nice opening of brackets.
let delimitMate_expand_cr=1 
let delimitMate_expand_space=1

" ------------ MARKDOWN -------------
" Disable automatic folding of markdown files.
let g:vim_markdown_folding_disabled = 1

" ------------ EVERYTHING HASKELL -------------
let g:haskell_conceal_wide = 1
let g:haskell_conceal_enumerations = 1
"let hscoptions="𝐒𝐓𝐄𝐌xRtB𝔻wr"
let hscoptions="℘𝐒𝐓𝐄𝐌xErbl↱w-iRtBQZ𝔻A"

" Show types in completion suggestions
let g:necoghc_enable_detailed_browse = 1
let g:haskellmode_completion_ghc = 1
autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc
let g:necoghc_debug = 1

" Disable hlint-refactor-vim's default keybindings
let g:hlintRefactor#disableDefaultKeybindings = 1


"    __                                   
"   / _  _ _  _ _. _   _ _|_|_. _  _  _   
"   \__)(-| )(-| |(_  _)(-|_|_|| )(_)_).  
"                                 _/     

set spell spelllang=en_gb

set history=1000         " Can't have too much history!  
set timeoutlen=400
<<<<<<< HEAD

set noswapfile	         " Don't keep swapfiles.
set undofile             " Keep an undo file (undo changes after closing).
set hls                  " Highlight search.
set ruler                " Show the cursor position all the time.
set showcmd              " Display incomplete commands in corner.
set number               " Show line number.
set relativenumber       " Relative line number.
set nobackup             " Use source control instead...
set incsearch            " Real time search highlighting.

set ignorecase smartcase " Clever searching
set nolazyredraw         " Fix rendering of terminal (?)
=======
 
set noswapfile	       " don't keep swapfiles.
set undofile           " keep an undo file (undo changes after closing)
set hls                " highlight search
set ruler              " show the cursor position all the time
set showcmd            " display incomplete commands
set number             " show line number
set relativenumber     " relative line number
set nobackup           " use source control instead...
set incsearch          " real time search highlighting

set ignorecase smartcase " clever searching
set lazyredraw           " better performance
>>>>>>> c8badadb943606a3700428bf03e7831658c49c85

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

" --------- NAVIGATION (SPLITS/WINDOWS/BUFFERS) ----------
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

" Use | or _ to open a vertical split or horizontal split.
nnoremap <expr><silent> <Bar> v:count == 0 ? "<C-W>v<C-W><Right>" : ":<C-U>normal! 0".v:count."<Bar><CR>"
nnoremap <expr><silent> _     v:count == 0 ? "<C-W>s<C-W><Down>"  : ":<C-U>normal! ".v:count."_<CR>"

" Use tab and shift-tab to cycle through windows in normal and terminal mode.
nnoremap <Tab> <C-W>w
nnoremap <S-Tab> <C-W>W

if has('nvim') " As above, exit terminal mode, and switch window.
  tnoremap <Tab>   <C-\><C-n><C-W>w
  tnoremap <S-Tab> <C-\><C-n><C-W>W
endif

" Move between tabs.
nnoremap <bs> :tabprevious<CR>
nnoremap <C-l> :tabnext<CR>

" Move around splits with <leader>-hjkl.
nnoremap <leader>j <C-W><C-J>
nnoremap <leader>k <C-W><C-K>
nnoremap <leader>l <C-W><C-L>
nnoremap <leader>h <C-W><C-H>

" Also move splits around with <leader>-HJKL.
" ('Pushes' split as far as possible in given direction.)
nnoremap <leader>J <C-W>J
nnoremap <leader>K <C-W>K
nnoremap <leader>L <C-W>L
nnoremap <leader>H <C-W>H

" Nerdtree.
map <leader>t :NERDTreeToggle<cr> 

" ----------- FIXES ------------
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

" ---------- VISUAL/UTILITY -----------
" Clears highlighting after search
nnoremap <leader>o :nohl<CR>

" Toggle spell check, and allow for sp-tab auto complete in normal mode
nmap <silent><leader>s :set spell!<CR> 
map <silent><leader><Tab> 1z=

" Save file with sp-w.
nnoremap <leader>w :w<cr>

<<<<<<< HEAD
=======
" Make Y work like it's supposed to.
nnoremap Y y$ 

" Use escape or qj to exit terminal mode.
if has('nvim')
  tnoremap <Esc> <C-\><C-n>
  tnoremap qj <C-\><C-n>

" Mappings for school computers...
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
"
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
endif

>>>>>>> c8badadb943606a3700428bf03e7831658c49c85
" Insert mode shows deterministic line no, normal mode shows relative.
autocmd InsertEnter * :set norelativenumber
autocmd InsertLeave * :set relativenumber

<<<<<<< HEAD
" Autosave all on focus lost (Terminal vim doesn't proc this but.. Eh)
autocmd FocusLost * :wa
=======
" Autosave on exiting insert mode.
autocmd InsertLeave * :w

>>>>>>> c8badadb943606a3700428bf03e7831658c49c85

" ---------- HASKELL OVERLORDS ------------
" Type of expression under cursor
nmap <silent> <leader>ht :GhcModType<CR>
" Insert type of expression under cursor
nmap <silent> <leader>hT :GhcModTypeInsert<CR>

" GHC errors and warnings
let g:neomake_haskell_ghc_mod_args = '-g-Wall'
nmap <silent> <leader>hc :Neomake ghcmod<CR>

" Hoogle search (shows type/ matching functions). 
" Search for word under cursor, or prompt for input
nnoremap <leader>hh :Hoogle<CR> 
nnoremap <leader>hH :Hoogle              

" Hoogle info (Detailed info for specific func)
" Search for word under cursor, or prompt for input
nnoremap <leader>hi :HoogleInfo<CR> 
nnoremap <leader>hI :HoogleInfo              

" Close the Hoogle window
nnoremap <silent> <leader>hq :HoogleClose<CR> 


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

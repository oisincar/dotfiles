
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
"Plug 'valloric/youcompleteme', { 'do': './install.py --all' }
Plug 'omnisharp/omnisharp-vim'
Plug 'scrooloose/syntastic'
Plug 'tpope/vim-dispatch'

Plug 'shougo/deoplete.nvim'

" -------- EDITING ----------
Plug 'Raimondi/delimitMate'
Plug 'godlygeek/tabular',       { 'for': 'markdown' } " Required for vim-markdown.
Plug 'plasticboy/vim-markdown', { 'for': 'markdown' }
Plug 'tpope/vim-commentary'
Plug 'benekastah/neomake'   " Used for haskell typechecking, but I should 
                            " use it for other stuff so it goes here..

Plug 'sheerun/vim-polyglot' " Syntax highlighting for every language imaginable.

" -------- COLOUR ----------
Plug 'ewilazarus/preto'
Plug 'fxn/vim-monochrome'
Plug 'robertmeta/nofrils'
Plug 'morhetz/gruvbox' 

" ------- NAVIGATION --------
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
Plug 'wellle/targets.vim'

Plug 'junegunn/fzf', { 'do': 'yes \| ./install'  }
Plug 'junegunn/fzf.vim'
Plug 'shougo/unite.vim'

" ---------- MISC -----------
Plug 'bling/vim-airline'
Plug 'tpope/vim-fugitive'

" -------- HASKELL ----------
Plug 'Shougo/vimproc.vim', 	            { 'do': 'make' }
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

" ------------- Syntastic/Csharp ---------------

" deoplete tab-complete
inoremap <silent><expr> <Tab> pumvisible() ? "\<C-n>" : deoplete#mappings#manual_complete()
" ,<Tab> for regular tab
inoremap <Leader><Tab> <Tab>

let g:syntastic_cs_checkers = ['syntax', 'semantic', 'issues']

let g:OmniSharp_server_type = 'roslyn'

" OmniSharp won't work without this setting
filetype plugin on

"This is the default value, setting it isn't actually necessary
let g:OmniSharp_host = "http://localhost:2000"

"Set the type lookup function to use the preview window instead of the status line
"let g:OmniSharp_typeLookupInPreview = 1

"Timeout in seconds to wait for a response from the server
let g:OmniSharp_timeout = 1

"Showmatch significantly slows down omnicomplete
"when the first match contains parentheses.
set noshowmatch

"don't autoselect first item in omnicomplete, show if only one item (for preview)
"remove preview if you don't want to see any documentation whatsoever.
set completeopt=longest,menuone,preview
" Fetch full documentation during omnicomplete requests.
" There is a performance penalty with this (especially on Mono)
" By default, only Type/Method signatures are fetched. Full documentation can still be fetched when
" you need it with the :OmniSharpDocumentation command.
" let g:omnicomplete_fetch_documentation=1

"Move the preview window (code documentation) to the bottom of the screen, so it doesn't move the code!
"You might also want to look at the echodoc plugin
set splitbelow

" Get Code Issues and syntax errors
let g:syntastic_cs_checkers = ['syntax', 'semantic', 'issues']
" If you are using the omnisharp-roslyn backend, use the following
" let g:syntastic_cs_checkers = ['code_checker']
augroup omnisharp_commands
    autocmd!

    "Set autocomplete function to OmniSharp (if not using YouCompleteMe completion plugin)
    autocmd FileType cs setlocal omnifunc=OmniSharp#Complete

    " Synchronous build (blocks Vim)
    "autocmd FileType cs nnoremap <F5> :wa!<cr>:OmniSharpBuild<cr>
    " Builds can also run asynchronously with vim-dispatch installed
    autocmd FileType cs nnoremap <leader>cb :wa!<cr>:OmniSharpBuildAsync<cr>
    " automatic syntax check on events (TextChanged requires Vim 7.4)
    "
    " Disabled due to annoying flickering..
    " autocmd BufEnter,TextChanged,InsertLeave *.cs SyntasticCheck

    " Automatically add new cs files to the nearest project on save
""    autocmd BufWritePost *.cs call OmniSharp#AddToProject()

    "show type information automatically when the cursor stops moving
    
    "HOLI SHHITTT this is cool vvv.
    "autocmd CursorHold *.cs call OmniSharp#TypeLookupWithoutDocumentation()

    "The following commands are contextual, based on the current cursor position.

    autocmd FileType cs nnoremap gd :OmniSharpGotoDefinition<cr>
    autocmd FileType cs nnoremap <leader>ci :OmniSharpFindImplementations<cr>
    autocmd FileType cs nnoremap <leader>ct :OmniSharpFindType<cr>
    autocmd FileType cs nnoremap <leader>cs :OmniSharpFindSymbol<cr>
    autocmd FileType cs nnoremap <leader>cu :OmniSharpFindUsages<cr>
    "finds members in the current buffer
    autocmd FileType cs nnoremap <leader>fm :OmniSharpFindMembers<cr>
    " cursor can be anywhere on the line containing an issue
    autocmd FileType cs nnoremap <leader>cx :OmniSharpFixIssue<cr>
    "autocmd FileType cs nnoremap <leader>fx :OmniSharpFixUsings<cr>
    autocmd FileType cs nnoremap <leader>tt :OmniSharpTypeLookup<cr>
    autocmd FileType cs nnoremap <leader>dc :OmniSharpDocumentation<cr>
    "navigate up by method/property/field
    autocmd FileType cs nnoremap <C-K> :OmniSharpNavigateUp<cr>
    "navigate down by method/property/field
    autocmd FileType cs nnoremap <C-J> :OmniSharpNavigateDown<cr>

augroup END


" this setting controls how long to wait (in ms) before fetching type / symbol information.
set updatetime=500
" Remove 'Press Enter to continue' message when type information is longer than one line.
set cmdheight=2

" Contextual code actions (requires CtrlP or unite.vim)
nnoremap <leader><space> :OmniSharpGetCodeActions<cr>
" Run code actions with text selected in visual mode to extract method
vnoremap <leader><space> :call OmniSharp#GetCodeActions('visual')<cr>

" " rename with dialog
" nnoremap <leader>nm :OmniSharpRename<cr>
" nnoremap <F2> :OmniSharpRename<cr>
" " rename without dialog - with cursor on the symbol to rename... ':Rename newname'
" command! -nargs=1 Rename :call OmniSharp#RenameTo("<args>")

" Force OmniSharp to reload the solution. Useful when switching branches etc.
nnoremap <leader>rl :OmniSharpReloadSolution<cr>
nnoremap <leader>cf :OmniSharpCodeFormat<cr>
" Load the current .cs file to the nearest project
nnoremap <leader>tp :OmniSharpAddToProject<cr>

" (Experimental - uses vim-dispatch or vimproc plugin) - Start the omnisharp server for the current solution
nnoremap <leader>ss :OmniSharpStartServer<cr>
nnoremap <leader>sp :OmniSharpStopServer<cr>

" Add syntax highlighting for types and interfaces
nnoremap <leader>th :OmniSharpHighlightTypes<cr>

"Don't ask to save when changing buffers (i.e. when jumping to a type definition)
set hidden

" ------------- YOUCOMPLETEME ---------------
if has('nvim')
    let g:Show_diagnostics_ui = 1 "default 1
    let g:YcmShowDetailedDiagnostic = 1
    let g:ycm_enable_diagnostic_signs = 1

    "let g:ycm_collect_identifiers_from_tags_files = 1 "default 0
    
    " (Haskell)
    let g:ycm_semantic_triggers = {'haskell' : ['.']}

    " Use symbols from file if (<4) chars, otherwise search full list of
    " functions. It's pretty hacky, but there doesn't seem to be a way 
    " to do combine symbols from the buffer and from haskell and necoghc.
    
    " let g:ycm_semantic_triggers.haskell = ['re!(?=[a-zA-Z_]{3})']
    
    " When the line above in commented out, semantic auto completion is triggered
    " as normal with <C-Space>, otherwise just completes with symbols from current buffer.
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

let g:haskellmode_completion_ghc = 0
autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc

" Highlight everything we got..
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

let g:fzf_action = {
      \ 'ctrl-s': 'split',
      \ 'ctrl-v': 'vsplit'
      \ }
"nnoremap <leader>f :FZF<CR>
nnoremap <leader>f :Files<CR>
nnoremap <leader>b :Buffers<CR>

" Toggle Nerdtree.
noremap <leader>t :NERDTreeToggle<cr> 

" Close vim if only window left open in nerdtree.
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

noremap <leader>rc :vsp ~/.vimrc<CR>

" ------ SPLITS/BUFFERS ---------
" Use | or _ to open a vertical split or horizontal split.
nnoremap <expr><silent> <Bar> v:count == 0 ? "<C-W>v<C-W><Right>" : ":<C-U>normal! 0".v:count."<Bar><CR>"
nnoremap <expr><silent> _     v:count == 0 ? "<C-W>s<C-W><Down>"  : ":<C-U>normal! ".v:count."_<CR>"

" Resize current split with +-
if bufwinnr(1)
  noremap + <C-W>+
  noremap - <C-W>-
endif

" Use tab and shift-tab to cycle through windows in normal and terminal mode.
nnoremap <S-Tab> <C-W>W

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

" Move around splits with <leader-w>-hjkl. Changed to be like spacemacs.
nnoremap <leader>wj <C-W><C-J>
nnoremap <leader>wk <C-W><C-K>
nnoremap <leader>wl <C-W><C-L>
nnoremap <leader>wh <C-W><C-H>

" Open terminal the same way as spacemacs; space '
nnoremap <leader>' :vsp<CR>:terminal<CR>

if has('nvim') " Enter insert mode when changing focus to a terminal split.
  autocmd BufWinEnter,WinEnter term://* startinsert
endif

" Also move splits around with <leader>-HJKL.
" ('Pushes' split as far as possible in given direction.)
nnoremap <leader>wJ <C-W>J
nnoremap <leader>wK <C-W>K
nnoremap <leader>wL <C-W>L
nnoremap <leader>wH <C-W>H

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


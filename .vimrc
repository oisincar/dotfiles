
"   ___  _     _       _              _                    
"  / _ \(_)___(_)_ __ ( )___   __   _(_)_ __ ___  _ __ ___ 
" | | | | / __| | '_ \|// __|  \ \ / / | '_ ` _ \| '__/ __|
" | |_| | \__ \ | | | | \__ \   \ V /| | | | | | | | | (__ 
"  \___/|_|___/_|_| |_| |___/  (_)_/ |_|_| |_| |_|_|  \___|
"

" Title font: big. Subtitle font: straight.
" http://patorjk.com/software/taag

" __                                          
"|__)|    _ . _  _   _  _  _|  |_|_  _ _  _ _ 
"|   ||_|(_)|| )_)  (_|| )(_|  |_| )(-|||(-_) 
"        _/                                   
let $nvim_tui_enable_true_color=1 " nvim true colour
set t_Co=256


if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')

" Completion
"if liteVersion:
if has('nvim')
    Plug 'valloric/youcompleteme'
endif
"Plug 'scrooloose/syntastic' 

" Editing
Plug 'Raimondi/delimitMate'
Plug 'godlygeek/tabular' " Required for vim-markdown.
Plug 'plasticboy/vim-markdown', { 'for': 'markdown' }
Plug 'tpope/vim-commentary'

" Colour
Plug 'ewilazarus/preto'
Plug 'fxn/vim-monochrome'
Plug 'robertmeta/nofrils'
Plug 'morhetz/gruvbox' 

" Navigation
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
Plug 'easymotion/vim-easymotion'

" Misc
Plug 'bling/vim-airline'
Plug 'tpope/vim-fugitive'

" Haskell
Plug 'neovimhaskell/haskell-vim',     { 'for': 'haskell' }
Plug 'enomsg/vim-haskellConcealPlus', { 'for': 'haskell' }
Plug 'eagletmt/ghcmod-vim',           { 'for': 'haskell' }
Plug 'bitc/vim-hdevtools',            { 'for': 'haskell' }
Plug 'eagletmt/neco-ghc',             { 'for': 'haskell' }
Plug 'Twinside/vim-hoogle',           { 'for': 'haskell' }
Plug 'mpickering/hlint-refactor-vim', { 'for': 'haskell' }

call plug#end()


" Colour scheme
set background=dark    " setting dark mode
let g:gruvbox_contrast_dark='hard'
colorscheme gruvbox

"colorscheme monochrome


" Airline (Status bar plugin) settings.
let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts = 1 
let g:airline_theme='gruvbox' 
let g:airline#extensions#wordcount#enabled = 1
let g:airline#extensions#whitespace#enabled = 0
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
endif

" delimitMate Options. Need to look into these more, for now just nice
" opening of brackets.
let delimitMate_expand_cr=1 
let delimitMate_expand_space=1

let mapleader="\<Space>"
" Nerdtree.
map <leader>t :NERDTreeToggle<cr> 


" __                                   
"/ _  _ _  _ _. _   _ _|_|_. _  _  _   
"\__)(-| )(-| |(_  _)(-|_|_|| )(_)_).  
"                              _/     

set spell spelllang=en_gb

set history=1000       " can't have too much history!  
set timeoutlen=400
 
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

" Highlight current line
set cursorline
set cmdheight=1
set switchbuf=useopen
set showtabline=2

" open new window splits to the right and bottom of current.
set splitbelow splitright

" keep 8 lines above or below the cursor when scrolling.
set scrolloff=8

" Keep 15 columns next to the cursor when scrolling horizontally.
set sidescroll=1
set sidescrolloff=15

" FUCK YOUR STUPID BELL STFU HOLY SHIT
set visualbell         

" smarttab. Set tab width to 4 spaces, and convert tabs to spaces.
set smarttab 
set tabstop=4 shiftwidth=4 expandtab 


"
"|_/ _  |_ . _  _|. _  _  _ 
"| \(-\/|_)|| )(_||| )(_)_) 
"     /               _/    

" Fix cursor bug, where it was moving onto next line. Nvim only.
"inoremap qj <Esc>`^	    
inoremap qj <Esc>
inoremap jq <Esc>

" Easier moving around wrapped lines
nmap j gj
nmap k gk

" Allows an undo of ctrl-u
inoremap <C-U> <C-G>u<C-U> 	

" Move around splits with <<leader>-hjkl>
nnoremap <leader>j <C-W><C-J>
nnoremap <leader>k <C-W><C-K>
nnoremap <leader>l <C-W><C-L>
nnoremap <leader>h <C-W><C-H>

" Move between tabs.
nnoremap <bs> :tabprevious<CR>
nnoremap <C-l> :tabnext<CR>

" Make a line above and below, then paste from buffer.
nnoremap <leader>sp o<CR><Esc>k<Esc>"+p

" Replace file with buffer. For proj euler.
nnoremap <leader>dp ggvG"+p

" My smart copy function.. Simply copies the entire file.
nnoremap <leader>c :%y+<CR>

" Toggle spell check, and allow for sp-tab auto complete in normal mode
nmap <silent><leader>s :set spell!<CR> 
map <silent><leader><Tab> 1z=

" Save file with sp-w.
nnoremap <leader>w :w<cr>

" Make Y work like it's supposed to.
nnoremap Y y$ 

" Use escape or qj to exit terminal mode.
if has('nvim')
  tnoremap <Esc> <C-\><C-n>
  tnoremap qj <C-\><C-n>

else
    noremap § $
    noremap 1 & 
    noremap 2 [
    noremap 3 {
    noremap 4 }
    noremap 5 (
    noremap 6 @
    noremap 7 *
    noremap 8 )
    noremap 9 ^
    noremap 0 ]
    noremap [ !
    noremap ] #

    noremap ± ~
    noremap ! % 
    noremap @ 7
    noremap # 5
    noremap $ 3
    noremap % 1
    noremap ^ 9
    noremap & 0
    noremap * 2
    noremap ( 4
    noremap ) 6
    noremap { 8
    noremap } `

    noremap ; '
    noremap : "
    noremap ' ;
    noremap " :
endif

" Insert mode shows deterministic line no, normal mode shows relative.
autocmd InsertEnter * :set norelativenumber
autocmd InsertLeave * :set relativenumber


"
"|\ | _ _ |_  (_    _  _|_. _  _  _ |   _ _ . _  _ .|_ _ 
"| \|(-(_||_  | |_|| )(_|_|(_)| )(_||  _)| )||_)|_)||__) 
"                                            |  |  

" Remember cursor's position in file, and set textwidth to 120.
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


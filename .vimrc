
"   ___  _     _       _              _                    
"  / _ \(_)___(_)_ __ ( )___   __   _(_)_ __ ___  _ __ ___ 
" | | | | / __| | '_ \|// __|  \ \ / / | '_ ` _ \| '__/ __|
" | |_| | \__ \ | | | | \__ \   \ V /| | | | | | | | | (__ 
"  \___/|_|___/_|_| |_| |___/  (_)_/ |_|_| |_| |_|_|  \___|
"

" Title font: big. Subtitle font: straight.
" http://patorjk.com/software/taag


" __               
"/ _  _ _  _ _ _ | 
"\__)(-| )(-| (_|| 
"                 

" Gotta do this first.
let mapleader="\<Space>"

syntax on                   " Switch syntax highlighting on

" __                                          
"|__)|    _ . _  _   _  _  _|  |_|_  _ _  _ _ 
"|   ||_|(_)|| )_)  (_|| )(_|  |_| )(-|||(-_) 
"        _/                                   

set nocompatible              " be improved, required
filetype off 
set rtp+=~/.vim/bundle/vundle.vim
call vundle#begin()

Plugin 'vundlevim/vundle.vim'

Plugin 'valloric/youcompleteme' 
Plugin 'rdnetto/ycm-generator'

Plugin 'scrooloose/nerdtree' 
"Plugin 'scrooloose/syntastic' 

Plugin 'godlygeek/tabular' " required for vim-markdown.
Plugin 'plasticboy/vim-markdown'
Plugin 'morhetz/gruvbox' 
Plugin 'bling/vim-airline'
Plugin 'tpope/vim-fugitive'


call vundle#end()            " required
filetype plugin indent on    " autodetect filetype and do indentation based on that.

" colour scheme
set background=dark    " setting dark mode
let g:gruvbox_contrast_dark='hard'
colorscheme gruvbox
let $nvim_tui_enable_true_color=1 " nvim true colour

" airline (thingy on bottom) settings.
let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts = 1

let g:airline_theme= 'gruvbox'


" YouCompleteMe options

let g:ycm_register_as_syntastic_checker = 1 "default 1
let g:Show_diagnostics_ui = 1 "default 1

"will put icons in Vim's gutter on lines that have a diagnostic set.
"Turning this off will also turn off the YcmErrorLine and YcmWarningLine
"highlighting
let g:ycm_enable_diagnostic_signs = 1
let g:ycm_enable_diagnostic_highlighting = 0
let g:ycm_always_populate_location_list = 1 "default 0
let g:ycm_open_loclist_on_ycm_diags = 1 "default 1


let g:ycm_complete_in_strings = 1 "default 1
let g:ycm_collect_identifiers_from_tags_files = 0 "default 0
let g:ycm_path_to_python_interpreter = '' "default ''


let g:ycm_server_use_vim_stdout = 0 "default 0 (logging to console)
let g:ycm_server_log_level = 'info' "default info


let g:ycm_global_ycm_extra_conf = '~/.ycm_extra_conf.py'  "where to search for .ycm_extra_conf.py if not found
let g:ycm_confirm_extra_conf = 0


let g:ycm_goto_buffer_command = 'same-buffer' "[ 'same-buffer', 'horizontal-split', 'vertical-split', 'new-tab' ]
let g:ycm_filetype_whitelist = { '*': 1 }
let g:ycm_key_invoke_completion = '<C-Space>'

"
" nerdtree.
map <leader>t :NERDTreeToggle<cr>

" __                                                      
"(_  _|_|_. _  _  _   _  _  _|  |  _  |_ . _  _|. _  _  _ 
"__)(-|_|_|| )(_)_)  (_|| )(_|  |((-\/|_)|| )(_||| )(_)_) 
"             _/                    /               _/    

set history=1000       " can't have too much history!  
set timeoutlen=400
" 
" vv Problem with saving and permisions. Disabled for now.
"set backup             " keep a backup file (restore to previous version)...
set noswapfile	       " don't keep swapfiles.
set undofile           " keep an undo file (undo changes after closing)
set hls                " highlight search
set ruler              " show the cursor position all the time
set showcmd            " display incomplete commands
set number             " show line number
set relativenumber     " relative line number

" highlight current line
set cursorline
set cmdheight=1
set switchbuf=useopen
set showtabline=2
set winwidth=79

" open new window splits to the right and bottom of current.
set splitbelow
set splitright

" keep 8 lines above or below the cursor when scrolling.
set scrolloff=8

" keep 15 columns next to the cursor when scrolling horizontally.
set sidescroll=1
set sidescrolloff=15

" smarttab. Set tab width to 4 spaces, and convert tabs to spaces.
set smarttab 
set tabstop=4 shiftwidth=4 expandtab 

" set a backup directory to get rid of the error. On new comupters this
" folder'll need to be re-created.
set backupdir=~/.vim/.backup//

set visualbell         " FUCK YOUR STUPID BELL STFU HOLY SHIT

inoremap qj <Esc>`^	    " Fix cursor bug, where it was moving onto next line.

" allows an undo of ctrl-u
inoremap <C-U> <C-G>u<C-U> 	

" toggle line numbers, relitive/deterministi 
nnoremap <C-n> :call NumberToggle()<cr> 

" enter breaks like it does in insertmode. 
 nnoremap <CR> i<CR><Esc>==

" move around splits with <<leader>-hjkl>
nnoremap <leader>j <C-W><C-J>
nnoremap <leader>k <C-W><C-K>
nnoremap <leader>l <C-W><C-L>
nnoremap <leader>h <C-W><C-H>

" move between tabs..
nnoremap <bs> :tabprevious<CR>
nnoremap <C-l> :tabnext<CR>

" <leader>sp is my smart paste function. Make space, then paste from outside buffer.
nnoremap <leader>sp o<CR><Esc>k<Esc>"+p

" Replace file with buffer. For proj euler.
nnoremap <leader>dv ggvG"+p

" My smart copy function.. Simply copies the entire file.
nnoremap <leader>c :%y+<CR>

" save file with sp-s.
nnoremap <leader>s :w<cr>

" make Y work like it's supposed to.
nnoremap Y y$ 
" use escape to exit terminal mode.
tnoremap <Esc> <C-\><C-n>

" insert mode shows deterministic line no, normal mode shows relative
autocmd InsertEnter * :set norelativenumber
autocmd InsertLeave * :set relativenumber

"
"|\ | _ _ |_  (_    _  _|_. _  _  _ |   _ _ . _  _ .|_ _ 
"| \|(-(_||_  | |_|| )(_|_|(_)| )(_||  _)| )||_)|_)||__) 
"                                            |  |  

" remember cursor's position in file, and set textwidth to 78.
augroup vimrcEx
  autocmd!
  autocmd FileType text setlocal textwidth=78
  autocmd BufReadPost *
    \ if line("'\"") > 1 && line("'\"") <= line("$") |
    \   execute "normal! g`\"" |
    \ endif
augroup END

" convenient command to see the difference between the current buffer and the
" file it was loaded from, thus the changes you made.
if !exists(":DiffOrig")
  command DiffOrig vert new | set buftype=nofile | read ++edit # | 0d_ | diffthis
                 \ | wincmd p | diffthis
endif

" toggle line number/relitive line number using <c-n>. Command listed above.
function! NumberToggle()
  if(&relativenumber == 1)
    set norelativenumber
  else
    set relativenumber
  endif
endfunc

" vp doesn't replace paste buffer
function! RestoreRegister()
  let @" = s:restore_reg
  return ''
endfunction
function! s:Repl()
  let s:restore_reg = @"
  return "p@=RestoreRegister()\<cr>"
endfunction
vmap <silent> <expr> p <sid>Repl()



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
filetype plugin indent on   " autodetect filetype and do indentation based on that.


" __                                          
"|__)|    _ . _  _   _  _  _|  |_|_  _ _  _ _ 
"|   ||_|(_)|| )_)  (_|| )(_|  |_| )(-|||(-_) 
"        _/                                   

"set rtp+=~/.vim/bundle/Vundle.vim
"call vundle#begin()

execute pathogen#infect()
colorscheme spacegray

" Plugin 'Valloric/YouCompleteMe'

" syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 1 


" toggle nerdtree.
map <leader>t :NERDTreeToggle<CR>

" autolaunch NERDTree on enter.
" autocmd vimenter * NERDTree


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

" don't use Ex mode, use Q for formatting
noremap Q gq			                 

" enter breaks like it does in insertmode. ,j breaks like o does, and k breaks
" like O does.
nnoremap <CR> i<CR><Esc>==
"nnoremap <leader>j o<Esc>
"nnoremap <leader>k O<Esc>

" move around splits with <<leader>-hjkl>
nnoremap <leader>j <C-W><C-J>
nnoremap <leader>k <C-W><C-K>
nnoremap <leader>l <C-W><C-L>
nnoremap <leader>h <C-W><C-H>

" <leader>sp is my smart paste function. Make space, then paste from outside buffer.
nnoremap <leader>sp o<CR><Esc>k<Esc>"+p

" save file with sp-s.
nnoremap <leader>s :w<cr>

" make Y work like it's supposed to.
nnoremap Y y$ 
" use escape to exit terminal mode.
"tnoremap <Esc> <C-\><C-n>

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


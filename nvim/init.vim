"dein Scripts-----------------------------
if &compatible
  set nocompatible               " Be iMproved
endif

" Required:
set runtimepath+=/home/lucas/.local/share/dein/repos/github.com/Shougo/dein.vim

" Required:
if dein#load_state('/home/lucas/.local/share/dein')
  call dein#begin('/home/lucas/.local/share/dein')

  " Let dein manage dein
  " Required:
  call dein#add('/home/lucas/.local/share/dein/repos/github.com/Shougo/dein.vim')
" dein Scripts-----------------------------

  " Add or remove your plugins here:
  call dein#add('Shougo/neosnippet.vim')
  call dein#add('Shougo/neosnippet-snippets')
  call dein#add('Shougo/deoplete.nvim')
  call dein#add('scrooloose/nerdtree')
  call dein#add('w0rp/ale')
  call dein#add('python-mode/python-mode')

  " You can specify revision/branch/tag.
  call dein#add('Shougo/vimshell', { 'rev': '3787e5' })

  " Required:
  call dein#end()
  call dein#save_state()
endif

" Required:
filetype plugin indent on
syntax enable

" If you want to install not installed plugins on startup.
if dein#check_install()
  call dein#install()
endif

"End dein Scripts-------------------------

" Color
set background=light  
colorscheme solarized

" Path
set runtimepath+=/home/lucas/.config/nvim/repos/github.com/Shougo/deoplete.nvim
set runtimepath+=/home/lucas/.config/nvim/repos/github.com/scrooloose/nerdtree

" Python
let g:python_host_prog = '/usr/bin/python'
let g:python3_host_prog = '/home/lucas/virtualenvs/env/bin/python3.6'

" Deoplete
call deoplete#enable()
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
autocmd CompleteDone * pclose " To close preview window of deoplete automagicall
let g:deoplete#disable_auto_complete = 1

" Yapf
autocmd FileType python nnoremap <leader>y :0,$!yapf<Cr>

" NERDTree
silent! map <F3> :NERDTreeToggle<CR>
silent! map <F2> :NERDTreeFind<CR>
let NERDTreeIgnore = ['\.pyc$']

" Window navigation
noremap <C-J> <C-W>w
noremap <C-K> <C-W>W
noremap <C-L> <C-W>l
noremap <C-H> <C-W>h

" python-mode
hi pythonSelf  ctermfg=68  guifg=#5f87d7 cterm=bold gui=bold
hi SpellBad cterm=Bold gui=Bold ctermfg=15 ctermbg=9 guifg=White guibg=Red     
hi IncSearch ctermbg=0
hi Search ctermbg=LightGray ctermfg=Black

" disable folding
set nofoldenable

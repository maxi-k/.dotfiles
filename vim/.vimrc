"--- THE VIM CUSTOMIZATIONS ---- "

set nocompatible 
filetype off

" Tell vim abount 256 colors
set t_Co=256

" Always show the status line
set laststatus=2

" Turn on syntax coloring
syntax on

" When editing a new file with the current buffer having
" unsaved changes, hide it insted of closing it
set hidden

" The current directory should always be that of the current file
set autochdir

" Enable line numbers
set relativenumber

" Relative line numbers when in normal (movement!) mode
" Absolute line numbers when in insert mode
autocmd InsertEnter * :set number norelativenumber
autocmd InsertLeave * :set relativenumber number

" Set tab width to 2
set tabstop=8 softtabstop=0 expandtab shiftwidth=2 smarttab

" Enable filetype plugins 
filetype indent plugin on 

" Uncomment to Swap colon and semicolon
" nnoremap ; :
" nnoremap : ;

" Open a new line with <Enter> in normal mode
" nmap <CR> i<CR><Esc>
" nmap <C-CR> m`o<Esc>``

" Make working with parens etc. nice
" inoremap { {<CR><BS>}<Esc>ko
" inoremap ( ()<Esc>i
" inoremap [ []<Esc>i
" inoremap <C-j> <Esc>/[)}"'\]>]<CR>:nohl<CR>a

" Purity
noremap <Up> <Nop>
noremap <Down> <Nop>
noremap <Left> <Nop>
noremap <Right> <Nop>

" Map jj to normal mode
imap jj <ESC>

" Map Y to act like D and C, i.e. to yank until EOL, rather than act as yy,
" which is the default
map Y y$

" Show partial commands on bottom right
set showcmd

" Highlight search matches
set hlsearch
set incsearch

" Activate smartcase for search, so that case sensitivity is `dwim`
" Toggling case-sensitivity works by appending \C or (in) \c at the end
set smartcase

" Map <C-L> (redraw screen) to also turn off search highlighting until the
" next search
nnoremap <C-L> :nohl<CR><C-L>

" When opening a new line and no filetype-specific indenting is enabled, keep
" the same indent as the line you're currently on. Useful for READMEs, etc.
set autoindent

" No annoying sounds
set errorbells visualbell
"
" Set the leader key to <space>
let mapleader=' '
" Test out the leader
nnoremap <leader>d dd

" No cursor blinking
set guicursor+=a:blinkon0

" Timeout before normal mode
set timeoutlen=0 ttimeoutlen=0

" Set the path to include recursive directories
" for fuzzy finding (:find filename)
set path+=**

" Tab completion for :find, with multiple results
" to choose from
set wildmenu

" ---- THE PLUGIN CUSTOMIZATIONS ---- "

" Include Vundle in the runtimepath and init 
set rtp+=~/.vim/bundle/Vundle.vim

"  Start Vundle
call vundle#begin()

" Let Vundle manage Vundle
Plugin 'gmarik/Vundle.vim'

" Lightline plugin
Plugin 'itchyny/lightline.vim'

" NerdTREE plugin
Plugin 'scrooloose/nerdtree'

" Org Mode clone for vim
Plugin 'jceb/vim-orgmode'

" Easymotion plugin
Plugin 'Lokaltog/vim-easymotion'

" Surround plugin
Plugin 'tpope/vim-surround'

" Easy Commenting
Plugin 'scrooloose/nerdcommenter'

" Syntastic linting plugin
Plugin 'scrooloose/syntastic'

" Supertab plugin
Plugin 'ervandew/supertab'

" Control-p plugin
Plugin 'kien/ctrlp.vim'

" Ctrl-Space
Plugin 'vim-ctrlspace/vim-ctrlspace'

" Find files with Spotlight
" Commands: :SpotEdit <file>, :SpotSplit <file>, :SpotSource <file>, :SpotRead <file>
Plugin 'vim-scripts/SpotlightOpen'

" Paredit. Must Have
" Plugin 'vim-scripts/paredit.vim'

" Parinfer. Replacement for Paredit
Plugin 'bhurlow/vim-parinfer'

" Vim Fireplace for clojure|script editing
Plugin 'tpope/vim-fireplace'

" Support for the elixir language
Plugin 'elixir-lang/vim-elixir'

" Support for the elm language
Plugin 'elmcast/elm-vim'

" Molokai theme
Plugin 'tomasr/molokai'

" End the Vundle init
call vundle#end()

" NetLogo Syntax
au! BufRead,BufNewFile *.nlogo setfiletype nlogo

" Easymotion
"map  <Leader>f <Plug>(easymotion-bd-f)
"nmap <Leader>f <Plug>(easymotion-overwin-f)

" Fix weird nerdtree chars
let g:NERDTreeDirArrows=0

" Map NerdTree to a key 
nnoremap <C-n> :NERDTreeToggle<CR>

" Keybinding for :SpotEdit <file>
nnoremap <C-s> :SpotEdit<space>

" Setup paredit
" let g:paredit_leader = '\<Space>'
" let g:paredit_electric_return = 0

" Setup org-mode filetypes
au BufNewFile,BufRead *.org set filetype=org

" Set the coloscheme to molokai
" which was downloaded with vundle
colorscheme molokai

" Make the font bigger
set guifont=Monaco:h15

" Make the background light
" set bg=light

" Make vim use the terminal background
" instead of inserting its own
highlight Normal ctermbg=NONE
highlight nonText ctermbg=NONE



"Specify plugin directory
call plug#begin('~/.local/share/nvim/plugged')

"Load plugins
Plug 'pangloss/vim-javascript'
Plug 'bling/vim-airline'
Plug 'bronson/vim-trailing-whitespace'
Plug 'airblade/vim-gitgutter'
Plug 'nathanaelkane/vim-indent-guides'
Plug 'ap/vim-css-color'
Plug 'grod/sublime-color-schemes.vim'
Plug 'editorconfig/editorconfig-vim'
Plug 'valloric/youcompleteme'
Plug 'valloric/matchtagalways'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'scrooloose/nerdtree'
Plug 'terryma/vim-multiple-cursors'

"Initialise plugin system
call plug#end()

"** Settings

"Theming
colorscheme Calydon

"Always show line numbers
set number

"Display line at column width 80
set colorcolumn=80

"Don't word wrap in the middle of words
set wrap linebreak nolist

"Keep indentation aligned when line wrapping
set breakindent

"Automatically read newly updated file in buffer
set autoread

"Exclude anything in .gitignore from fuzzy find
let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files -co --exclude-standard']


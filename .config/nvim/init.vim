"Specify plugin directory
call plug#begin('~/.local/share/nvim/plugged')

Plug 'pangloss/vim-javascript'
Plug 'bling/vim-airline'
Plug 'bronson/vim-trailing-whitespace'
Plug 'airblade/vim-gitgutter'
Plug 'nathanaelkane/vim-indent-guides'
Plug 'ap/vim-css-color'
Plug 'grod/sublime-color-schemes.vim'
Plug 'editorconfig/editorconfig-vim'
Plug 'valloric/youcompleteme'
Plug 'ctrlpvim/ctrlp.vim'

"Initialise plugin system
call plug#end()

"** Settings

"Exclude anything in .gitignore from fuzzy find
let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files -co --exclude-standard']


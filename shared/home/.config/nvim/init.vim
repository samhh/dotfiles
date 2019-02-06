"Specify plugin directory
call plug#begin('~/.local/share/nvim/plugged')

"Load plugins
Plug 'bronson/vim-trailing-whitespace'
Plug 'airblade/vim-gitgutter'
Plug 'nathanaelkane/vim-indent-guides'
Plug 'editorconfig/editorconfig-vim'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'terryma/vim-multiple-cursors'
Plug 'tpope/vim-commentary'
Plug 'farmergreg/vim-lastplace'
Plug 'myusuf3/numbers.vim'
Plug 'lifepillar/vim-gruvbox8'

"Initialise plugin system
call plug#end()

"** Settings

"Theming
colorscheme gruvbox8
hi Normal guibg=NONE ctermbg=NONE

"Statusline
set laststatus=2

function! StatusLineGit()
  let l:branchname = system("git rev-parse --abbrev-ref HEAD 2>/dev/null | tr -d '\n'")
  return strlen(l:branchname) > 0?'  '.l:branchname.' ':''
endfunction

set statusline=
set statusline+=%#PmenuSel#
set statusline+=%{StatusLineGit()}
set statusline+=%#LineNr#
set statusline+=\ %f
set statusline+=%m
set statusline+=%=
set statusline+=\ %#CursorColumn#
set statusline+=\ %y
set statusline+=\ %l:%c
set statusline+=\ 

"Indent based upon file's indentation
filetype plugin indent on

"Tabs to spaces and sizing
set tabstop=8
set expandtab
set shiftwidth=2
set softtabstop=2

"Use fish as internal shell
set shell=fish

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

"Fix editing crontab
au BufEnter /private/tmp/crontab.* setl backupcopy=yes

"Keybinds
""Disable arrow keys because old habits die hard
noremap <Up> <NOP>
noremap <Down> <NOP>
noremap <Left> <NOP>
noremap <Right> <NOP>


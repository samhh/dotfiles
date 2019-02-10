"Plugins
call plug#begin('~/.local/share/nvim/plugged')

Plug 'bronson/vim-trailing-whitespace'
Plug 'airblade/vim-gitgutter'
Plug 'nathanaelkane/vim-indent-guides'
Plug 'editorconfig/editorconfig-vim'
Plug 'terryma/vim-multiple-cursors'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'
Plug 'justinmk/vim-sneak'
Plug 'myusuf3/numbers.vim'
Plug 'lifepillar/vim-gruvbox8'

call plug#end()

"Theming
colorscheme gruvbox8
hi Normal guibg=NONE ctermbg=NONE

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

"Folding
set foldmethod=syntax

"Fix editing crontab
au BufEnter /private/tmp/crontab.* setl backupcopy=yes

"Keybinds
""Disable arrow keys because old habits die hard
noremap <Up> <NOP>
noremap <Down> <NOP>
noremap <Left> <NOP>
noremap <Right> <NOP>

""Convenient buffer selection
nnoremap gb :ls<CR>:b<Space>

""Add newlines from normal mode
""Note that the shift enter bind doesn't appear to work
noremap <Enter> o<Esc>k
noremap <S-Enter> O<Esc>

""Toggle to last open file in buffer
noremap <Space> <C-^>

""Remove highlight
nnoremap <C-l> :nohl<CR><C-l>

"Statusline
set laststatus=2

function! CountBuffer()
  return len(filter(copy(getbufinfo()), 'v:val.listed'))
endfunction

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
set statusline+=\ %n
set statusline+=/%{CountBuffer()}

"Markdown preview
function! OpenMarkdownPreview()
  if !exists('s:markdown_preview_job')
    let s:markdown_preview_job = jobstart('grip')
  endif
  silent exec '!open http://localhost:6419/' . expand('%')
endfunction

noremap <silent> <leader>md :call OpenMarkdownPreview()<cr>


" Plugins
call plug#begin('~/.local/share/nvim/plugged')

"" General
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

"" Color schemes
Plug 'lifepillar/vim-gruvbox8'

"" Language syntax
Plug 'pangloss/vim-javascript'
Plug 'leafgarland/typescript-vim'
Plug 'ianks/vim-tsx'

"" Language servers
" let g:coc_start_at_startup = 0
" Plug 'neoclide/coc.nvim'
Plug 'neoclide/coc.nvim', {'tag': '*', 'do': './install.sh'}

call plug#end()

" Theming
colorscheme gruvbox8
hi Normal guibg=NONE ctermbg=NONE

" Indent based upon file's indentation
filetype plugin indent on

" Tabs to spaces and sizing
set tabstop=8
set expandtab
set shiftwidth=2
set softtabstop=2

" Path for finding files
set path=**

" File searching
set wildmenu
set wildignore+=**/node_modules/**

" Use fish as internal shell
set shell=fish

" Always show line numbers
set number

" Display line at column width 80
set colorcolumn=80

" Don't word wrap in the middle of words
set wrap linebreak nolist

" Keep indentation aligned when line wrapping
set breakindent

" Allow hidden buffers (for coc)
set hidden

" Automatically read newly updated file in buffer
set autoread

" Disable any expensive folding on load to improve performance of massive files
" Enable folding manually with foldmethod `syntax` or `indent`
set foldmethod=manual
set nofoldenable

" Keybinds

"" Disable arrow keys because old habits die hard
nnoremap <Up> <NOP>
nnoremap <Down> <NOP>
nnoremap <Left> <NOP>
nnoremap <Right> <NOP>

"" Convenient buffer selection
nnoremap gb :ls<CR>:b<Space>

"" Add newlines from normal mode
"" Note that the shift enter bind doesn't appear to work
nnoremap <Enter> o<Esc>k
nnoremap <S-Enter> O<Esc>

"" Toggle to last open file in buffer
nnoremap <Space> <C-^>

"" Remove highlight
nnoremap <C-l> :nohl<CR><C-l>

"" Navigate coc diagnostics
nmap <silent> [c <Plug>(coc-diagnostic-prev)
nmap <silent> ]c <Plug>(coc-diagnostic-next)

"" coc gotos
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

"" Rename coc symbol
nmap <leader>rn <Plug>(coc-rename)

" Statusline
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

" Markdown preview
function! OpenMarkdownPreview()
  if !exists('s:markdown_preview_job')
    let s:markdown_preview_job = jobstart('grip')
  endif
  silent exec '!open http://localhost:6419/' . expand('%')
endfunction

noremap <silent> <leader>md :call OpenMarkdownPreview()<cr>


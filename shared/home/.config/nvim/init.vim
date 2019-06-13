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
Plug 'rstacruz/vim-closer'

"" Color schemes
Plug 'arcticicestudio/nord-vim'
Plug 'itchyny/lightline.vim'

"" Language syntax
Plug 'pangloss/vim-javascript'
Plug 'leafgarland/typescript-vim'
Plug 'ianks/vim-tsx'

"" Language servers
" let g:coc_start_at_startup = 0
Plug 'neoclide/coc.nvim', {'tag': '*', 'do': './install.sh'}

call plug#end()

" Theming
colorscheme nord
let g:lightline = { 'colorscheme': 'nord' }
set noshowmode

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
set wildignore+=*/node_modules/*,*/target/*

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
nnoremap <silent> <space>a  :<C-u>CocList diagnostics<cr>

"" coc gotos
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

"" Rename coc symbol
nmap <leader>rn <Plug>(coc-rename)

"" Trigger coc completions
inoremap <silent><expr> <c-space> coc#refresh()

"" Show documentation (type info) in preview window
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Markdown preview
function! OpenMarkdownPreview()
  if !exists('s:markdown_preview_job')
    let s:markdown_preview_job = jobstart('grip')
  endif
  silent exec '!open http://localhost:6419/' . expand('%')
endfunction

noremap <silent> <leader>md :call OpenMarkdownPreview()<cr>


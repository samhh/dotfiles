" Plugins
call plug#begin('~/.local/share/nvim/plugged')

"" General
Plug 'bronson/vim-trailing-whitespace'
Plug 'airblade/vim-gitgutter'
Plug 'nathanaelkane/vim-indent-guides'
Plug 'editorconfig/editorconfig-vim'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'justinmk/vim-sneak'
Plug 'rstacruz/vim-closer'
Plug 'moll/vim-bbye'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
""" The first of these lines is required on macOS
Plug '/usr/local/opt/fzf'
Plug 'junegunn/fzf.vim'

"" Color schemes
Plug 'arcticicestudio/nord-vim'
Plug 'itchyny/lightline.vim'

"" Language syntax
Plug 'pangloss/vim-javascript'
Plug 'leafgarland/typescript-vim'
Plug 'peitalin/vim-jsx-typescript'
Plug 'purescript-contrib/purescript-vim'
Plug 'neovimhaskell/haskell-vim'

call plug#end()

" Theming
colorscheme nord
let g:lightline = {
\    'colorscheme': 'nord',
\    'active': {
\        'left': [ [ 'mode', 'paste' ], [ 'readonly', 'filename', 'modified' ] ],
\        'right': [ [ 'lineinfo' ], [ 'percent' ], [ 'buffer' ], [ 'filetype' ], [ 'cocstatus' ] ],
\    },
\    'component': {
\        'buffer': "%{len(filter(range(1,bufnr('$')),'buflisted(v:val)'))}b",
\    },
\    'component_function': {
\        'cocstatus': 'coc#status',
\    },
\ }
autocmd User CocStatusChange,CocDiagnosticChange call lightline#update()
set noshowmode

" Indent based upon file's indentation
filetype plugin indent on

" Tabs to spaces and sizing
set tabstop=8
set expandtab
set shiftwidth=4
set softtabstop=4

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
let mapleader = ' '

"" Disable arrow keys because old habits die hard
nnoremap <Up> <NOP>
nnoremap <Down> <NOP>
nnoremap <Left> <NOP>
nnoremap <Right> <NOP>

"" Buffer selection
nnoremap <Leader>b :Buffers<CR>

"" Toggle to last open file in buffer
nnoremap <Leader><Bs> <C-^>

"" Delete buffer (and use plugin for it to preserve layouts)
nnoremap <Leader>q :Bd<CR>

"" Add newlines from normal mode
"" Note that the shift enter bind doesn't appear to work
nnoremap <Enter> o<Esc>k
nnoremap <S-Enter> O<Esc>

"" Remove highlight
nnoremap <C-l> :noh<CR>

"" Fuzzy find files
nnoremap <Leader>p :GFiles<CR>
nnoremap <Leader>P :Files<CR>
nnoremap <Leader>l :<C-U>call ABFiles()<CR>

"" Custom fzf find files in directory of active buffer
function! ABFiles()
  execute 'FZF' expand('%:p:h')
endfunction

"" Fuzzy find code
nnoremap <Leader>f :Lines<CR>
nnoremap <Leader>F :Rg<CR>

""" Customised fzf.vim Rg implementation to ignore lockfiles
command! -bang -nargs=* Rg
  \ call fzf#vim#grep(
  \   'rg -g "!*.lock" --column --line-number --no-heading --color=always --smart-case '.shellescape(<q-args>), 1,
  \   <bang>0 ? fzf#vim#with_preview('up:60%')
  \           : fzf#vim#with_preview('right:50%:hidden', '?'),
  \   <bang>0)

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
nmap <Leader>rn <Plug>(coc-rename)

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


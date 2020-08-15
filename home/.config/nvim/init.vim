" Plugins
if exists('*minpac#init')
    call minpac#init()
    call minpac#add('k-takata/minpac', {'type': 'opt'})

    "" General
    call minpac#add('tpope/vim-sleuth')
    call minpac#add('bronson/vim-trailing-whitespace')
    call minpac#add('airblade/vim-gitgutter')
    call minpac#add('nathanaelkane/vim-indent-guides')
    call minpac#add('editorconfig/editorconfig-vim')
    call minpac#add('tpope/vim-commentary')
    call minpac#add('tpope/vim-surround')
    call minpac#add('rstacruz/vim-closer')
    call minpac#add('moll/vim-bbye')
    call minpac#add('neoclide/coc.nvim', {'branch': 'release'})
    call minpac#add('junegunn/fzf.vim')

    "" Theming
    call minpac#add('arcticicestudio/nord-vim')
    call minpac#add('itchyny/lightline.vim')

    "" Language syntax
    call minpac#add('pangloss/vim-javascript')
    call minpac#add('leafgarland/typescript-vim')
    call minpac#add('peitalin/vim-jsx-typescript')
    call minpac#add('purescript-contrib/purescript-vim')
    call minpac#add('neovimhaskell/haskell-vim')
    call minpac#add('cespare/vim-toml')
endif

command! PackUpdate packadd minpac | source $MYVIMRC | call minpac#update('', {'do': 'call minpac#status()'})
command! PackClean  packadd minpac | source $MYVIMRC | call minpac#clean()

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

" Write swap file faster so that plugins like gitgutter are more responsive
set updatetime=50

" Always leave space for sign column so (dis)appearance of signs in a buffer
" doesn't cause the text to shift
set signcolumn=yes

" Live substitution
set inccommand=nosplit

" Highlight references to symbol under cursor
autocmd CursorHold * silent call CocActionAsync('highlight')

" Keybinds
let mapleader = ' '

"" Disable arrow keys because old habits die hard
nnoremap <Up> <NOP>
nnoremap <Down> <NOP>
nnoremap <Left> <NOP>
nnoremap <Right> <NOP>

"" Disable unneeded Ex mode bind that's easy to mistakenly hit
nnoremap Q <NOP>

"" Remap a questionable default for more consistency with C and D
nnoremap Y y$

"" Buffer selection
nnoremap <Leader>b :Buffers<CR>

"" Toggle to last open file in buffer
nnoremap <Leader>v <C-^>

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
""" In directory of active buffer
nnoremap <Leader>l :execute 'FZF' expand('%:p:h')<CR>

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
nmap <silent> <Leader>k <Plug>(coc-diagnostic-prev)
nmap <silent> <Leader>j <Plug>(coc-diagnostic-next)
nnoremap <silent> <Leader>a :<C-u>CocList diagnostics<cr>

"" coc gotos
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

"" Rename coc symbol
nmap <Leader>r <Plug>(coc-rename)

"" Symbols search
nnoremap <silent> <Leader>s :<C-u>CocList outline<cr>
nnoremap <silent> <Leader>S :<C-u>CocList symbols<cr>

"" Trigger coc completions
inoremap <silent><expr> <C-space> coc#refresh()

"" Format active buffer
nnoremap <silent> <Leader>z :call CocAction('format')<cr>

"" Show documentation (type info) in preview window
nnoremap <silent> K :call CocAction('doHover')<CR>


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

" Disable folding on load by default to improve performance in massive files
set nofoldenable
set foldmethod=manual

" Write swap file faster so that plugins like gitgutter are more responsive
set updatetime=50

" Always leave space for sign column so (dis)appearance of signs in a buffer
" doesn't cause the text to shift
set signcolumn=yes

" Live substitution
set inccommand=nosplit

" Remove default scratch/preview window from autocomplete
set completeopt=menuone,noinsert

augroup TerminalBehavior
  " Remove number/sign columns in terminal
  autocmd TermOpen * setlocal nonumber signcolumn=no
  " Immediately enter insert mode in terminal
  autocmd TermOpen * startinsert
augroup END

" Configure LSP client
lua << EOF
  vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
    vim.lsp.diagnostic.on_publish_diagnostics, {
      virtual_text = false,
      signs = true,
      update_in_insert = false,
      underline = true,
    }
  )
EOF


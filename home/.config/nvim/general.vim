" Always show line numbers
set number

" Display line at column width 80
set colorcolumn=80

" Don't word wrap in the middle of words
set wrap linebreak nolist

" Keep indentation aligned when line wrapping
set breakindent

" Allow background closing/hiding of buffers with unsaved changes
set hidden

" Automatically read newly updated file in buffer
set autoread

" Disable folding on load by default to improve performance in massive files
set nofoldenable
set foldmethod=manual
set foldexpr=nvim_treesitter#foldexpr()

" Write swap file faster so that plugins like gitgutter are more responsive
set updatetime=50

" Display signs in number column
set signcolumn=number

" Live substitution
set inccommand=nosplit

" Remove default scratch/preview window from autocomplete
set completeopt=menuone,noinsert

" Differentiate LSP diagnostics styling
autocmd ColorScheme * highlight LspDiagnosticsUnderlineError guifg=red ctermfg=red cterm=underline gui=underline
autocmd ColorScheme * highlight LspDiagnosticsUnderlineWarning guifg=yellow ctermfg=yellow cterm=underline gui=underline
autocmd ColorScheme * highlight LspDiagnosticsUnderlineInformation guifg=none ctermfg=none cterm=underline gui=underline
autocmd ColorScheme * highlight LspDiagnosticsUnderlineHint guifg=none ctermfg=none cterm=underline gui=underline

" Highlight references to symbol under cursor
autocmd ColorScheme  * lua require('vim.lsp.diagnostic')._define_default_signs_and_highlights()
autocmd ColorScheme  * highlight LspReferenceRead ctermbg=0
autocmd CursorHold   * lua vim.lsp.buf.document_highlight()
autocmd CursorHoldI  * lua vim.lsp.buf.document_highlight()
autocmd CursorMoved  * lua vim.lsp.buf.clear_references()
autocmd CursorMovedI * lua vim.lsp.buf.clear_references()

augroup TerminalBehavior
  " Remove number/sign columns in terminal
  autocmd TermOpen * setlocal nonumber signcolumn=no
  " Immediately enter insert mode in terminal
  autocmd TermOpen * startinsert
augroup END


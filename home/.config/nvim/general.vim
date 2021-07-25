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

" Use ripgrep and ignore lockfiles
set grepprg=rg\ -S\ -g\ \"!*.lock\"\ --vimgrep\ $*

" Always prompt for an explicit completion choice
set completeopt=menuone,noselect

" Suppress completion messages
set shortmess+=c

" Differentiate LSP diagnostics styling
autocmd ColorScheme * highlight LspDiagnosticsUnderlineError guifg=red ctermfg=red cterm=underline gui=underline
autocmd ColorScheme * highlight LspDiagnosticsUnderlineWarning guifg=yellow ctermfg=yellow cterm=underline gui=underline
autocmd ColorScheme * highlight LspDiagnosticsUnderlineInformation guifg=none ctermfg=none cterm=underline gui=underline
autocmd ColorScheme * highlight LspDiagnosticsUnderlineHint guifg=none ctermfg=none cterm=underline gui=underline

" Highlight references to symbol under cursor (conditionally enabled elsewhere)
autocmd ColorScheme  * lua require('vim.lsp.diagnostic')._define_default_signs_and_highlights()
autocmd ColorScheme  * highlight LspReferenceRead ctermbg=0

" Auto-open quickfix window
autocmd QuickFixCmdPost [^l]* nested cwindow
autocmd QuickFixCmdPost    l* nested lwindow

" Briefly highlight on yank
autocmd TextYankPost * lua vim.highlight.on_yank { higroup = "IncSearch", timeout = 150, on_visual = true }


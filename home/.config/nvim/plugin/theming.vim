colorscheme wal

function! DiagnosticsStatus() abort
    let l:es = luaeval("vim.lsp.diagnostic.get_count(0, [[Error]])")
    let l:ws = luaeval("vim.lsp.diagnostic.get_count(0, [[Warning]])")

    return l:es + l:ws == 0 ? 'ok' : printf('%dw %de', ws, es)
endfunction

let g:lightline = {
\    'active': {
\        'left': [ [ 'mode', 'paste' ], [ 'readonly', 'filename', 'modified' ] ],
\        'right': [ [ 'percent', 'lineinfo' ], [ 'filetype', 'buffers' ], [ 'diagnostics' ] ],
\    },
\    'component': {
\        'buffers': "%{len(filter(range(1,bufnr('$')),'buflisted(v:val)'))}b",
\    },
\    'component_function': {
\        'diagnostics': 'DiagnosticsStatus',
\    },
\ }

set noshowmode

" Differentiate LSP diagnostics
autocmd ColorScheme * highlight LspDiagnosticsUnderlineError guifg=red ctermfg=red cterm=underline gui=underline
autocmd ColorScheme * highlight LspDiagnosticsUnderlineWarning guifg=yellow ctermfg=yellow cterm=underline gui=underline
autocmd ColorScheme * highlight LspDiagnosticsUnderlineInformation guifg=none ctermfg=none cterm=underline gui=underline
autocmd ColorScheme * highlight LspDiagnosticsUnderlineHint guifg=none ctermfg=none cterm=underline gui=underline
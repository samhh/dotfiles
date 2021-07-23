colorscheme wal

function! DiagnosticsStatus() abort
    let l:es = luaeval("vim.lsp.diagnostic.get_count(0, [[Error]])")
    let l:ws = luaeval("vim.lsp.diagnostic.get_count(0, [[Warning]])")

    return l:es + l:ws == 0 ? 'ok' : printf('%dw %de', ws, es)
endfunction

let g:lightline = {
\    'active': {
\        'left': [ [ 'mode', 'paste' ], [ 'readonly', 'filename', 'modified' ] ],
\        'right': [ [ 'lineinfo' ], [ 'percent' ], [ 'buffers' ], [ 'filetype' ], [ 'diagnostics' ] ],
\    },
\    'component': {
\        'buffers': "%{len(filter(range(1,bufnr('$')),'buflisted(v:val)'))}b",
\    },
\    'component_function': {
\        'diagnostics': 'DiagnosticsStatus',
\    },
\ }

set noshowmode


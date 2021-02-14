colorscheme nord

function! DiagnosticsStatus() abort
    let l:lint = ale#statusline#Count(bufnr(''))
    let l:lint_es = l:lint.error + l:lint.style_error
    let l:lint_ws = l:lint.total - l:lint_es

    let l:es = l:lint_es + luaeval("vim.lsp.diagnostic.get_count(0, [[Error]])")
    let l:ws = l:lint_ws + luaeval("vim.lsp.diagnostic.get_count(0, [[Warning]])")

    return l:es + l:ws == 0 ? 'ok' : printf('%dw %de', ws, es)
endfunction

let g:lightline = {
\    'colorscheme': 'nord',
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


lua <<EOF
  require'kanagawa'.setup {
    transparent = true,
    commentStyle = "NONE",
    functionStyle = "NONE",
    keywordStyle = "NONE",
    statementStyle = "NONE",
    typeStyle = "NONE",
    variablebuiltinStyle = "NONE",
    specialException = false,
    specialReturn = false,
  }
EOF

colorscheme kanagawa

function! DiagnosticsStatus() abort
    let l:es = luaeval("#vim.diagnostic.get(0, { severity = vim.diagnostic.severity.ERROR })")
    let l:ws = luaeval("#vim.diagnostic.get(0, { severity = vim.diagnostic.severity.WARN })")

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

colorscheme nord

function! DiagnosticsStatus() abort
    let l:counts = ale#statusline#Count(bufnr(''))

    let l:all_errors = l:counts.error + l:counts.style_error
    let l:all_non_errors = l:counts.total - l:all_errors

    return l:counts.total == 0 ? 'ok' : printf(
    \   '%dw %de',
    \   all_non_errors,
    \   all_errors
    \)
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
let g:limelight_conceal_ctermfg = 'gray'


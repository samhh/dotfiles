setlocal omnifunc=v:lua.vim.lsp.omnifunc
setlocal foldmethod=indent
setlocal makeprg=cabal
let b:ale_linters = []
let b:ale_fixers = ['stylish-haskell']


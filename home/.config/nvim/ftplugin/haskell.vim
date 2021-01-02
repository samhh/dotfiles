setlocal omnifunc=v:lua.vim.lsp.omnifunc
setlocal foldenable
setlocal foldmethod=indent
setlocal makeprg=cabal
let b:ale_linters = ['hlint']
let b:ale_fixers = ['stylish-haskell']


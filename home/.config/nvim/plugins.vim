let g:ale_disable_lsp = 1

packadd minpac
call minpac#init()
call minpac#add('k-takata/minpac', {'type': 'opt'})

" General
call minpac#add('tpope/vim-sleuth')
call minpac#add('bronson/vim-trailing-whitespace')
call minpac#add('airblade/vim-gitgutter')
call minpac#add('nathanaelkane/vim-indent-guides')
call minpac#add('editorconfig/editorconfig-vim')
call minpac#add('tpope/vim-commentary')
call minpac#add('tpope/vim-surround')
call minpac#add('rstacruz/vim-closer')
call minpac#add('moll/vim-bbye')
call minpac#add('vim-test/vim-test')
call minpac#add('dense-analysis/ale')
call minpac#add('junegunn/fzf.vim')

" LSP
call minpac#add('neovim/nvim-lspconfig')
lua <<EOF
  require'lspconfig'.tsserver.setup{}
  require'lspconfig'.hls.setup{}
  require'lspconfig'.purescriptls.setup{}

  vim.cmd([[ autocmd ColorScheme * :lua require('vim.lsp.diagnostic')._define_default_signs_and_highlights() ]])

  vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
    vim.lsp.diagnostic.on_publish_diagnostics, {
      virtual_text = false,
      signs = true,
      update_in_insert = false,
      underline = true,
    }
  )
EOF

" Writing
call minpac#add('junegunn/goyo.vim')
call minpac#add('junegunn/limelight.vim')

" Theming
call minpac#add('arcticicestudio/nord-vim')
call minpac#add('itchyny/lightline.vim')

" Language syntax
call minpac#add('herringtondarkholme/yats.vim')
call minpac#add('purescript-contrib/purescript-vim')
call minpac#add('neovimhaskell/haskell-vim')
call minpac#add('cespare/vim-toml')
call minpac#add('vmchale/dhall-vim')

command! PackUpdate call minpac#update()
command! PackClean  call minpac#clean()


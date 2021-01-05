let g:ale_disable_lsp = 1
let g:ale_echo_cursor = 0

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

call minpac#add('nvim-lua/popup.nvim')
call minpac#add('nvim-lua/plenary.nvim')
call minpac#add('nvim-telescope/telescope.nvim')
lua <<EOF
  local actions = require'telescope.actions'
  local previewers = require('telescope.previewers')

  require'telescope'.setup {
    defaults = {
      mappings = {
        i = {
          ["<esc>"] = actions.close,
        },
      },
      file_previewer = previewers.vim_buffer_cat.new,
      grep_previewer = previewers.vim_buffer_vimgrep.new,
      qflist_previewer = previewers.vim_buffer_qflist.new,
    },
  }
EOF

call minpac#add('nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'})
lua <<EOF
  require'nvim-treesitter.configs'.setup {
    ensure_installed = "maintained",
    highlight = { enable = true },
    indent = { enable = true },
  }
EOF

" LSP
call minpac#add('neovim/nvim-lspconfig')
call minpac#add('nathunsmitty/nvim-ale-diagnostic')
lua <<EOF
  require'lspconfig'.tsserver.setup{}
  require'lspconfig'.hls.setup{}
  require'lspconfig'.purescriptls.setup{}

  vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
    vim.lsp.diagnostic.on_publish_diagnostics,
    {
      virtual_text = false,
      signs = true,
      update_in_insert = false,
      underline = false,
    }
  )
EOF

" Writing
call minpac#add('junegunn/goyo.vim')
call minpac#add('junegunn/limelight.vim')

" Theming
call minpac#add('arcticicestudio/nord-vim')
call minpac#add('itchyny/lightline.vim')

" Language syntax (unsupported by treesitter)
call minpac#add('purescript-contrib/purescript-vim')
call minpac#add('neovimhaskell/haskell-vim')
call minpac#add('vmchale/dhall-vim')

command! PackUpdate call minpac#update()
command! PackClean  call minpac#clean()


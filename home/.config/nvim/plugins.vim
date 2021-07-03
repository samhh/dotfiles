let g:ale_disable_lsp = 1

function! PackInit() abort
  packadd minpac
  call minpac#init()
  call minpac#add('k-takata/minpac', {'type': 'opt'})

  " General
  call minpac#add('ii14/exrc.vim')

  " Layout
  call minpac#add('moll/vim-bbye')
  call minpac#add('airblade/vim-gitgutter')
  call minpac#add('nathanaelkane/vim-indent-guides')
  call minpac#add('bronson/vim-trailing-whitespace')

  " Motions
  call minpac#add('tpope/vim-commentary')
  call minpac#add('mhinz/vim-grepper')
  call minpac#add('tpope/vim-surround')

  " Formatting
  call minpac#add('editorconfig/editorconfig-vim')
  call minpac#add('tpope/vim-sleuth')

  " File management
  call minpac#add('junegunn/fzf.vim')

  " Compilation
  call minpac#add('tpope/vim-dispatch')

  " LSP & linting
  call minpac#add('dense-analysis/ale')
  call minpac#add('hrsh7th/nvim-compe')
  call minpac#add('neovim/nvim-lspconfig')

  " Syntax
  call minpac#add('nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'})
  call minpac#add('nvim-treesitter/nvim-treesitter-refactor')
  call minpac#add('derekelkins/agda-vim')
  call minpac#add('vmchale/dhall-vim')
  call minpac#add('neovimhaskell/haskell-vim')
  call minpac#add('purescript-contrib/purescript-vim')

  " Theming
  call minpac#add('dylanaraps/wal.vim')
  call minpac#add('itchyny/lightline.vim')
endfunction

lua <<EOF
  require'nvim-treesitter.configs'.setup {
    ensure_installed = "maintained",
    highlight = { enable = true },
    indent = { enable = true },
    autotag = { enable = true },
    context_commentstring = { enable = true },
    refactor = {
      highlight_definitions = { enable = true },
      navigation = {
        enable = true,
        keymaps = {
          goto_definition = "gld"
        },
      },
      smart_rename = {
        enable = true,
        keymaps = {
          smart_rename = "glr"
        },
      },
    },
  }

  local lspc = require'lspconfig'

  local servers = { "bashls", "gopls", "hls", "purescriptls", "rls", "tsserver" }
  for _, lsp in ipairs(servers) do
    lspc[lsp].setup { on_attach = on_attach }
  end

  vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
    vim.lsp.diagnostic.on_publish_diagnostics,
    {
      virtual_text = false,
      signs = true,
      update_in_insert = false,
      underline = true,
    }
  )

  require'compe'.setup {
    enabled = true;
    autocomplete = false;
    documentation = true;

    source = {
      nvim_lsp = true;
      path = true;
      buffer = true;
    };
  }
EOF

command! PackUpdate call PackInit() | call minpac#update()
command! PackClean  call PackInit() | call minpac#clean()


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
  call minpac#add('phaazon/hop.nvim')

  " Formatting
  call minpac#add('editorconfig/editorconfig-vim')
  call minpac#add('tpope/vim-sleuth')

  " File management
  call minpac#add('junegunn/fzf.vim')

  " Compilation
  call minpac#add('tpope/vim-dispatch')

  " LSP
  call minpac#add('neovim/nvim-lspconfig')

  " Syntax
  call minpac#add('nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'})
  call minpac#add('nvim-treesitter/nvim-treesitter-refactor')
  call minpac#add('vmchale/dhall-vim')
  call minpac#add('neovimhaskell/haskell-vim')
  call minpac#add('purescript-contrib/purescript-vim')
  call minpac#add('dag/vim-fish')

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

  local function concat_tables(xs, ys)
    local zs = {}

    for _,x in ipairs(xs) do table.insert(zs, x) end
    for _,y in ipairs(ys) do table.insert(zs, y) end

    return zs
  end

  local function table_has_value(xs, y)
    for i,x in ipairs(xs) do
      if x == y then return true end
    end

    return false
  end

  -- Use LSP-enhanced keybinds when available
  local function setup_keybinds()
    vim.api.nvim_buf_set_option(0, 'omnifunc', 'v:lua.vim.lsp.omnifunc')
    vim.api.nvim_buf_set_keymap(0, 'i', '<C-Space>', '<C-x><C-o>', { noremap = true })

    vim.api.nvim_buf_set_keymap(0, 'n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', { noremap = true })
  end

  local function disable_server_fmt(client)
    client.resolved_capabilities.document_formatting = false
  end

  local servers_fmt = { "bashls", "gopls", "purescriptls", "rls" }
  local servers_nofmt = { "hls", "tsserver" }
  local servers = concat_tables(servers_fmt, servers_nofmt)

  for _, server in ipairs(servers) do
    lspc[server].setup {
      on_attach = function(client)
        setup_keybinds()
        if table_has_value(servers_nofmt, server) then disable_server_fmt(client) end
      end
    }
  end

  lspc.efm.setup {
    filetypes = { "haskell", "javascript", "typescript", "typescriptreact" }
  }

  vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
    vim.lsp.diagnostic.on_publish_diagnostics,
    {
      virtual_text = false,
      signs = true,
      update_in_insert = false,
      underline = true,
    }
  )
EOF

command! PackUpdate call PackInit() | call minpac#update()
command! PackClean  call PackInit() | call minpac#clean()


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
  call minpac#add('rstacruz/vim-closer')
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
  call minpac#add('nvim-treesitter/nvim-treesitter', { 'branch': '0.5-compat', 'do': ':TSUpdate' })
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

  local function setup_treesitter()
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
            goto_definition_lsp_fallback = "gd"
          },
        },
        smart_rename = {
          enable = true,
          keymaps = {
            smart_rename = "<Leader>r"
          },
        },
      },
    }
  end

  local function setup_lsp_servers()
    local lspc = require'lspconfig'

    -- Use LSP-enhanced keybinds when available
    local function setup_keybinds()
      vim.api.nvim_buf_set_option(0, 'omnifunc', 'v:lua.vim.lsp.omnifunc')
      vim.api.nvim_buf_set_keymap(0, 'i', '<C-n>', '<C-x><C-o>', { noremap = true })
      vim.api.nvim_buf_set_keymap(0, 'n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', { noremap = true })
      vim.api.nvim_buf_set_keymap(0, 'n', '<Leader>r', '<cmd>lua vim.lsp.buf.rename()<CR>', { noremap = false })
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
  end

  local function setup_lsp_diags()
    vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
      vim.lsp.diagnostic.on_publish_diagnostics,
      {
        virtual_text = false,
        signs = true,
        update_in_insert = false,
        underline = true,
      }
    )
  end

  local function setup_lsp_qf()
    local pubdiag = "textDocument/publishDiagnostics"
    local def_pubdiag_handler = vim.lsp.handlers[pubdiag]
    vim.lsp.handlers[pubdiag] = function(err, method, res, cid, bufnr, cfg)
      def_pubdiag_handler(err, method, res, cid, bufnr, cfg)

      local qfdiags = {}
      for bufnr_, diags in pairs(vim.lsp.diagnostic.get_all()) do
        for _, diag in ipairs(diags) do
          diag.bufnr = bufnr_
          diag.lnum = diag.range.start.line + 1
          diag.col = diag.range.start.character + 1
          diag.text = diag.message
          table.insert(qfdiags, diag)
        end
      end
      vim.lsp.util.set_qflist(qfdiags)
    end
  end

  setup_treesitter()
  setup_lsp_servers()
  setup_lsp_diags()
  setup_lsp_qf()
EOF

command! PackUpdate call PackInit() | call minpac#update()
command! PackClean  call PackInit() | call minpac#clean()


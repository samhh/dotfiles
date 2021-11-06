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

  local function this_buf_set_option(opt, value)
    vim.api.nvim_buf_set_option(0, opt, value)
  end

  local function this_buf_set_keymap(mode, bind, effect)
    vim.api.nvim_buf_set_keymap(0, mode, bind, effect, { noremap = true })
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
    local lspc_cfgs = require'lspconfig/configs'

    -- Use LSP-enhanced keybinds when available
    local function setup_keybinds()
      this_buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')
      this_buf_set_keymap('i', '<Tab>', '<C-x><C-o>')
      this_buf_set_keymap('n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>')
      this_buf_set_keymap('n', '<Leader>r', '<cmd>lua vim.lsp.buf.rename()<CR>')
    end

    local function attacher_fmt(client)
        setup_keybinds()
    end

    local function attacher_nofmt(client)
        setup_keybinds()
        client.resolved_capabilities.document_formatting = false
    end

    lspc.bashls.setup {
      on_attach = attacher_fmt
    }

    if not lspc_cfgs.cssmodules then
        lspc_cfgs.cssmodules = {
            default_config = {
                cmd = {'cssmodules-language-server'},
                filetypes = {'javascript', 'javascriptreact', 'typescript', 'typescriptreact'},
                init_options = {
                    camelCase = true,
                },
                root_dir = require('lspconfig.util').root_pattern('package.json')
            }
        }
    end

    lspc_cfgs.cssmodules.setup {
      on_attach = attacher_nofmt
    }

    lspc.efm.setup {
      filetypes = { "haskell", "javascript", "typescript", "typescriptreact" }
    }

    lspc.eslint.setup {
      on_attach = attacher_fmt
    }

    lspc.gopls.setup {
      on_attach = attacher_fmt
    }

    lspc.hls.setup {
      on_attach = attacher_nofmt
    }

    lspc.purescriptls.setup {
      on_attach = attacher_fmt
    }

    lspc.rls.setup {
      on_attach = attacher_fmt
    }

    lspc.tsserver.setup {
      init_options = {
        plugins = { {
          name = "typescript-tshm-plugin",
          location = "/usr/lib/node_modules/typescript-tshm-plugin/"
        } }
      },
      on_attach = attacher_nofmt
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

  require'hop'.setup()

  require'telescope'.setup()
  require'telescope'.load_extension('fzf')
EOF


local k = require('keymap')

local pickers = require('telescope.builtin')
local themes = require('telescope.themes')

local function this_buf_set_option(opt, value)
  vim.api.nvim_buf_set_option(0, opt, value)
end

local function setup_servers()
  local lspc = require'lspconfig'

  -- Use LSP-enhanced keybinds when available
  local function setup_keybinds()
    this_buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')
    k.buf.mapi('<C-u>', '<C-x><C-o>')
    k.buf.mapn('K', function() vim.lsp.buf.hover() end)
    k.buf.mapn('<Leader>r', function() vim.lsp.buf.rename() end)
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

  lspc.dhall_lsp_server.setup {
    on_attach = attacher_fmt
  }

  lspc.efm.setup {
    filetypes = { "javascript", "typescript", "typescriptreact" }
  }

  lspc.eslint.setup {
    on_attach = attacher_fmt
  }

  lspc.hls.setup {
    on_attach = attacher_fmt,
    settings = { haskell = { formattingProvider = "stylish-haskell" } }
  }

  lspc.purescriptls.setup {
    on_attach = attacher_fmt
  }

  lspc.rust_analyzer.setup {
    on_attach = attacher_fmt
  }

  lspc.rnix.setup {
    on_attach = attacher_fmt
  }

  lspc.tsserver.setup {
    init_options = {
      plugins = { {
        name = "typescript-tshm-plugin",
        location = "/home/sam/.local/share/typescript-tshm-plugin/"
      } }
    },
    on_attach = attacher_nofmt,
    -- Patch to solve goto definition in React opening quickfix:
    --   - https://github.com/neovim/neovim/issues/14556
    --   - https://github.com/typescript-language-server/typescript-language-server/issues/216
    --   - https://github.com/microsoft/TypeScript/issues/37816
    --
    -- Plucks the first result and drops the rest. The API should support
    -- `A | [A] | nil`, making this safe.
    handlers = {
      ["textDocument/definition"] = function (e, xs, ...)
        vim.lsp.handlers['textDocument/definition'](err, xs[1], ...)
      end
    }
  }
end

local function setup_diags()
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

local function setup_ll()
  local pubdiag = "textDocument/publishDiagnostics"
  local def_pubdiag_handler = vim.lsp.handlers[pubdiag]
  vim.lsp.handlers[pubdiag] = function(err, method, res, cid, bufnr, cfg)
    def_pubdiag_handler(err, method, res, cid, bufnr, cfg)
    vim.diagnostic.setloclist({ open = false })
  end
end

setup_servers()
setup_diags()
setup_ll()

-- Goto reference
k.mapn('gr', function() vim.lsp.buf.references() end)

-- Local symbol search
k.mapn('gs', function() pickers.treesitter() end)

-- Format/fix active buffer
k.mapn('<Leader>z', function() vim.lsp.buf.formatting() end)

-- Show diagnostics in popup
k.mapn('<Leader>e', function() vim.diagnostic.open_float() end)

-- Offer code actions
k.mapn('<Leader>g', function() vim.lsp.buf.code_action() end)

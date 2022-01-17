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
    k.buf.mapn('K', '<cmd>lua vim.lsp.buf.hover()<CR>')
    k.buf.mapn('<Leader>r', '<cmd>lua vim.lsp.buf.rename()<CR>')
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

local function setup_qf()
  local pubdiag = "textDocument/publishDiagnostics"
  local def_pubdiag_handler = vim.lsp.handlers[pubdiag]
  vim.lsp.handlers[pubdiag] = function(err, method, res, cid, bufnr, cfg)
    def_pubdiag_handler(err, method, res, cid, bufnr, cfg)

    local qfdiags = {}
    for bufnr_, diags in pairs(vim.diagnostic.get()) do
      for _, diag in ipairs(diags) do
        -- Filter out deprecation diagnostics.
        if not string.match(diag.message, "deprecated") then
          diag.bufnr = bufnr_
          diag.lnum = diag.range.start.line + 1
          diag.col = diag.range.start.character + 1
          diag.text = diag.message
          table.insert(qfdiags, diag)
        end
      end
    end
    vim.fn.setqflist(qfdiags)
  end
end

setup_servers()
setup_diags()
setup_qf()

-- Goto reference
k.mapn('gr', '<Cmd>lua vim.lsp.buf.references()<CR>')

-- Symbols search
_G.syml = function() pickers.lsp_document_symbols() end
k.mapn('gs', '<Cmd>lua syml()<CR>')
k.mapn('gS', '<Cmd>lua vim.lsp.buf.workspace_symbol()<CR>')

-- Format/fix active buffer
k.mapn('<Leader>z', '<Cmd>lua vim.lsp.buf.formatting()<CR>')

-- Show diagnostics in popup
k.mapn('<Leader>e', '<Cmd>lua vim.diagnostic.open_float(0)<CR>')

-- Offer code actions
_G.act = function() pickers.lsp_code_actions(themes.get_cursor()) end
k.mapn('<Leader>g', '<Cmd>lua act()<CR>')

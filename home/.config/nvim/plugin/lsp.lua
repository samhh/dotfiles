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
    -- Specifically borrowed the below code from:
    --   - https://github.com/typescript-language-server/typescript-language-server/issues/216#issuecomment-939369240
    handlers = {
      ["textDocument/definition"] = function (_, results, params)
        if results == nil or vim.tbl_isempty(results) then
           local _ = vim.lsp.log.info() and vim.lsp.log.info(params.method, 'No location found')
           return nil
        end

        if vim.tbl_islist(results) then
           vim.lsp.util.jump_to_location(results[1])
           if #results > 1 then
              local isReactDTs = false

              for _, result in pairs(results) do
                 if string.match(result.uri, "react/index.d.ts") then
                    isReactDTs = true
                    break
                 end
              end

              if not isReactDTs then
                 vim.lsp.util.set_qflist(util.locations_to_items(results))
                 vim.api.nvim_command("copen")
                 vim.api.api.nvim_command("wincmd p")
              end
           end
        else
           vim.lsp.util.jump_to_location(results)
        end
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
    for bufnr_, diags in pairs(vim.lsp.diagnostic.get_all()) do
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
    vim.lsp.util.set_qflist(qfdiags)
  end
end

setup_servers()
setup_diags()
setup_qf()

-- Goto reference
k.mapn('gr', '<Cmd>lua vim.lsp.buf.references()<CR>')

-- Symbols search
_G.syml = function() pickers.lsp_document_symbols(themes.get_cursor()) end
k.mapn('gs', '<Cmd>lua syml()<CR>')
k.mapn('gS', '<Cmd>lua vim.lsp.buf.workspace_symbol()<CR>')

-- Format/fix active buffer
k.mapn('<Leader>z', '<Cmd>lua vim.lsp.buf.formatting()<CR>')

-- Show diagnostics in popup
k.mapn('<Leader>e', '<Cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>')

-- Offer code actions
_G.act = function() pickers.lsp_code_actions(themes.get_cursor()) end
k.mapn('<Leader>g', '<Cmd>lua act()<CR>')

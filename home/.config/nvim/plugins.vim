let g:ale_disable_lsp = 1

packadd minpac
call minpac#init()
call minpac#add('k-takata/minpac', {'type': 'opt'})

" General
call minpac#add('ii14/exrc.vim')
call minpac#add('tpope/vim-dispatch')
call minpac#add('tpope/vim-sleuth')
call minpac#add('bronson/vim-trailing-whitespace')
call minpac#add('airblade/vim-gitgutter')
call minpac#add('nathanaelkane/vim-indent-guides')
call minpac#add('editorconfig/editorconfig-vim')
call minpac#add('tpope/vim-commentary')
call minpac#add('tpope/vim-surround')
call minpac#add('moll/vim-bbye')
call minpac#add('dense-analysis/ale')
call minpac#add('mhinz/vim-grepper')
call minpac#add('junegunn/fzf.vim')
call minpac#add('nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'})
lua <<EOF
  require'nvim-treesitter.configs'.setup {
    ensure_installed = "maintained",
    highlight = { enable = true },
    indent = { enable = true },
    autotag = { enable = true },
    context_commentstring = { enable = true },
  }
EOF

" LSP
call minpac#add('neovim/nvim-lspconfig')
call minpac#add('hrsh7th/nvim-compe')
lua <<EOF
  local lspc = require'lspconfig'

  local servers = { "bashls", "gopls", "hls", "purescriptls", "rls", "tsserver" }

  -- Conditionally enable highlighting references under cursor according to
  -- language server capabilities
  local on_attach = function(client, bufnr)
    if client.resolved_capabilities.document_highlight then
      vim.api.nvim_exec([[
        augroup lsp_document_highlight
          autocmd! * <buffer>
          autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight()
          autocmd CursorHoldI <buffer> lua vim.lsp.buf.document_highlight()
          autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()
          autocmd CursorMovedI <buffer> lua vim.lsp.buf.clear_references()
        augroup END
      ]], false)
    end
  end

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

" Theming
call minpac#add('arcticicestudio/nord-vim')
call minpac#add('itchyny/lightline.vim')

" Language syntax (where not already supported via treesitter)
call minpac#add('purescript-contrib/purescript-vim')
call minpac#add('neovimhaskell/haskell-vim')
call minpac#add('vmchale/dhall-vim')
call minpac#add('derekelkins/agda-vim')

command! PackUpdate call minpac#update()
command! PackClean  call minpac#clean()


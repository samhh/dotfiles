-- Workaround for:
--   https://github.com/NixOS/nixpkgs/issues/189838#issuecomment-1301609540
local parser_install_dir = vim.fn.stdpath("cache") .. "/treesitters"
vim.fn.mkdir(parser_install_dir, "p")
vim.opt.runtimepath:append(parser_install_dir)

require'nvim-treesitter.configs'.setup {
  parser_install_dir = parser_install_dir,
  ensure_installed = "all",
  highlight = { enable = true },
  indent = { enable = false },
  autotag = { enable = true },
  context_commentstring = { enable = true },
  matchup = { enable = false },
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


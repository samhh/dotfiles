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


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

setup_treesitter()

require'Comment'.setup()
require'hop'.setup()

local actions = require('telescope.actions')
require'telescope'.setup {
  defaults = {
    mappings = {
      i = {
        ["<Esc>"] = actions.close,
        ["<C-c>"] = function() vim.cmd [[stopinsert]] end
      }
    }
  }
}
require'telescope'.load_extension('fzf')

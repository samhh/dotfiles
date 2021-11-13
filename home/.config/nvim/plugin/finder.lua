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

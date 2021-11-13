local k = require('keymap')

local actions = require('telescope.actions')

require'telescope'.setup {
  defaults = {
    mappings = {
      -- Quick exit with an escape hatch for normal mode
      i = {
        ["<Esc>"] = actions.close,
        ["<C-c>"] = function() vim.cmd [[stopinsert]] end
      }
    }
  }
}

require'telescope'.load_extension('fzf')

-- Buffer selection
k.mapn(
  '<Leader>b',
  '<Cmd>lua require \'telescope.builtin\'.buffers {}<CR>'
)

-- Find in repo
k.mapn(
  '<Leader>P',
  '<Cmd>lua require \'telescope.builtin\'.git_files { previewer = false }<CR>'
)

-- Find in directory of open buffer
k.mapn(
  '<Leader>l',
  '<Cmd>lua require \'telescope.builtin\'.find_files { previewer = false, search_dirs = { vim.fn.expand(\'%:h\') } }<CR>'
)

-- Find by path option
function _G.get_telescope_paths()
  local vim_paths = vim.opt.path:get()
  local telescope_paths = {}

  for _,p in ipairs(vim_paths) do
    -- Paths will look something like this:
    --   { "", ",", "client/**", "server/**" }
    -- The first two are defaults which shouldn't cause any harm. The latter
    -- two are the only pattern I use anywhere, that is "path/to/dir/**". For
    -- compatibility with Telescope we simply need to remove the "**" suffix.
    local x, _ = string.gsub(p, "%**", "")
    table.insert(telescope_paths, x)
  end

  return telescope_paths
end

k.mapn(
  '<Leader>p',
  '<Cmd>lua require \'telescope.builtin\'.find_files { previewer = false, search_dirs = get_telescope_paths() }<CR>'
)

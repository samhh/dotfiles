local Keymap = {
  buf = {},
}

local function mapIn(m)
  return function(l, r)
    vim.api.nvim_set_keymap(
      m,
      l,
      r,
      { noremap = true }
    )
  end
end

local function mapInBuf(b, m)
  return function(l, r)
    vim.api.nvim_buf_set_keymap(
      b,
      m,
      l,
      r,
      { noremap = true }
    )
  end
end

Keymap.mapn = mapIn('n')
Keymap.mapi = mapIn('i')
Keymap.buf.mapn = mapInBuf(0, 'n')
Keymap.buf.mapi = mapInBuf(0, 'i')

return Keymap

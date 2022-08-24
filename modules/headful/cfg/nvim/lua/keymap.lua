local Keymap = {
  buf = {},
}

local function mapIn(m)
  return function(l, r)
    vim.keymap.set(
      m,
      l,
      r
    )
  end
end

local function mapInBuf(b, m)
  return function(l, r)
    vim.keymap.set(
      m,
      l,
      r,
      { buffer = b }
    )
  end
end

Keymap.mapn = mapIn('n')
Keymap.mapi = mapIn('i')
Keymap.buf.mapn = mapInBuf(0, 'n')
Keymap.buf.mapi = mapInBuf(0, 'i')

return Keymap

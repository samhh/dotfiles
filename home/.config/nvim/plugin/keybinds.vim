let mapleader = ' '

" Disable unneeded Ex mode bind that's easy to mistakenly hit
nnoremap Q <NOP>

" Remap a questionable default for more consistency with C and D
nnoremap Y y$

" Buffer selection
nnoremap <Leader>b :ls<CR>:b<Space>

" Toggle to last open buffer
nnoremap <Leader>v <C-^>

" Delete buffer, preserving layout if not quickfix list or :help
nnoremap <Leader>q :Bd<CR>
autocmd FileType qf,help nnoremap <buffer> <Leader>q :bd<CR>

" Add newlines from normal mode
nnoremap <CR> o<Esc>k
nnoremap <S-CR> O<Esc>

"" Don't override the default <CR> bind in the quickfix list
autocmd BufReadPost quickfix nnoremap <buffer> <CR> <CR>

" Remove highlight
nnoremap <Leader>h :noh<CR>

" Find by path option
lua <<EOF
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

  vim.api.nvim_set_keymap(
    'n',
    '<Leader>p',
    '<Cmd>lua require \'telescope.builtin\'.find_files { search_dirs = get_telescope_paths() }<CR>',
    { noremap = true }
  )
EOF

" Find in repo
nnoremap <Leader>P <Cmd>lua require 'telescope.builtin'.git_files {}<CR>

" Find in directory of open buffer
nnoremap <Leader>l <Cmd>lua require 'telescope.builtin'.find_files { search_dirs = { vim.fn.expand('%:h') } }<CR>

" Hop to word
nnoremap gh :HopWord<CR>

" :grep motions and shortcut
let g:grepper = {}
let g:grepper.quickfix = 0
nmap gs <plug>(GrepperOperator)
nnoremap <Leader>/ :silent lgrep ''<Left>

" Async make
nnoremap <Leader>m :Make 

" Push git changes to quickfix
"" Disable vim-gitgutter's default mappings
let g:gitgutter_map_keys = 0
nnoremap <Leader>y :GitGutterQuickFix<CR>

" Navigate location list
nmap <silent> gL :lprev<CR>
nmap <silent> gl :lnext<CR>

" Navigate quickfix list
nmap <silent> gF :cprev<CR>
nmap <silent> gf :cnext<CR>

" Navigate jumplist
nmap <silent> gJ <C-o>
nmap <silent> gj <C-i>

" LSP gotos
nmap <silent> gr <Cmd>lua vim.lsp.buf.references()<CR>

" Symbols search
nnoremap <silent> <Leader>s <Cmd>lua vim.lsp.buf.document_symbol()<CR>
nnoremap <silent> <Leader>S <Cmd>lua vim.lsp.buf.workspace_symbol()<CR>

" Format/fix active buffer
nnoremap <silent> <Leader>z <Cmd>lua vim.lsp.buf.formatting()<CR>

" Show diagnostics in popup
nnoremap <silent> <Leader>e <Cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>

" Offer code actions
nnoremap <silent> <Leader>g <Cmd>lua vim.lsp.buf.code_action()<CR>


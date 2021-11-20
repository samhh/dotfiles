" Use two spaces by default - these are overridden by editorconfig where
" applicable
set expandtab
set softtabstop=2
set shiftwidth=2

" Always show line numbers
set number

" Display line at column width 80
set colorcolumn=80

" Don't word wrap in the middle of words
set linebreak

" Keep indentation aligned when line wrapping
set breakindent

" Allow background closing/hiding of buffers with unsaved changes
set hidden

" Disable folding on load by default to improve performance in massive files
set nofoldenable
set foldmethod=manual
set foldexpr=nvim_treesitter#foldexpr()

" Display signs in number column
set signcolumn=number

" Keep n lines of context visible at all times
set scrolloff=5

" Live substitution
set inccommand=nosplit

" Only shallow search in editor's PWD
set path=,,

" Use ripgrep and ignore lockfiles
set grepprg=rg\ -S\ -g\ \"!*.lock\"\ --vimgrep\ $*

" Always prompt for an explicit completion choice
set completeopt=menuone,noselect

" Suppress completion messages
set shortmess+=c

" Auto-open quickfix window
autocmd QuickFixCmdPost [^l]* nested cwindow
autocmd QuickFixCmdPost    l* nested lwindow

" Briefly highlight on yank
autocmd TextYankPost * lua vim.highlight.on_yank { higroup = "IncSearch", timeout = 150, on_visual = true }

" Auto-trust exrc on creation
autocmd BufNewFile .exrc nested autocmd BufWritePost <buffer> nested ExrcTrust

" Initialise plugins
lua <<EOF
  require'Comment'.setup()
  require'hop'.setup()
  require'gitsigns'.setup {
    -- Imlicitly also disables all (other) default keybinds
    keymaps = {
      noremap = true,

      ['o ih'] = ':<C-U>lua require"gitsigns.actions".select_hunk()<CR>',
      ['x ih'] = ':<C-U>lua require"gitsigns.actions".select_hunk()<CR>'
    }
  }
EOF


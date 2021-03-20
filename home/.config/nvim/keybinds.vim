let mapleader = ' '

" Disable arrow keys because old habits die hard
nnoremap <Up> <NOP>
nnoremap <Down> <NOP>
nnoremap <Left> <NOP>
nnoremap <Right> <NOP>

" Disable unneeded Ex mode bind that's easy to mistakenly hit
nnoremap Q <NOP>

" Remap a questionable default for more consistency with C and D
nnoremap Y y$

" Buffer selection
nnoremap <Leader>b :Buffers<cr>

" Toggle to last open file in buffer
nnoremap <Leader>v <C-^>

" Delete buffer (and use plugin for it to preserve layouts)
nnoremap <Leader>q :Bd<cr>

" Add newlines from normal mode
nnoremap <Enter> o<Esc>k
nnoremap <S-Enter> O<Esc>

" Remove highlight
nnoremap <C-l> :noh<cr>

" Fuzzy find filenames
nnoremap <Leader>p :GFiles<cr>
nnoremap <Leader>P :Files<cr>
"" In directory of active buffer
nnoremap <Leader>l :execute 'FZF' expand('%:p:h')<cr>

" Fuzzy find text
nnoremap <Leader>f :Lines<cr>
nnoremap <Leader>F :Rg<cr>

"" Customised fzf.vim Rg implementation to ignore lockfiles
command! -bang -nargs=* Rg
  \ call fzf#vim#grep(
  \   'rg -g "!*.lock" --column --line-number --no-heading --color=always --smart-case '.shellescape(<q-args>), 1,
  \   <bang>0 ? fzf#vim#with_preview('up:60%')
  \           : fzf#vim#with_preview('right:50%:hidden', '?'),
  \   <bang>0)

" Navigate location list
nmap <silent> <Leader>j :llist<cr>
nmap <silent> <C-k> :labove<cr>
nmap <silent> <C-j> :lbelow<cr>

" LSP gotos
nmap <silent> gd <cmd>lua vim.lsp.buf.definition()<cr>
nmap <silent> gr <cmd>lua vim.lsp.buf.references()<cr>

" Rename LSP symbol
nmap <Leader>r <cmd>lua vim.lsp.buf.rename()<cr>

" Symbols search
nnoremap <silent> <Leader>s <cmd>lua vim.lsp.buf.document_symbol()<cr>
nnoremap <silent> <Leader>S <cmd>lua vim.lsp.buf.workspace_symbol()<cr>

" Trigger LSP completions
inoremap <silent> <C-space> <C-x><C-o>

" Format/fix active buffer
nnoremap <silent> <Leader>z :ALEFix<cr>

" Show documentation (type info) in preview window
nnoremap <silent> <Leader>d <cmd>lua vim.lsp.buf.hover()<cr>

" Show diagnostics in popup
nnoremap <silent> <Leader>e <cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<cr>

" Offer code actions
nnoremap <silent> <Leader>g <cmd>lua vim.lsp.buf.code_action()<cr>

" Digraphs
dig NN 8469  " ℕ
dig fi 8718  " ∎
dig .- 8760  " ∸
dig (< 10216 " ⟨
dig >) 10217 " ⟩


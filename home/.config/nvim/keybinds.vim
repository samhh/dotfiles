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
nnoremap <Leader>b :Buffers<CR>

" Toggle to last open file in buffer
nnoremap <Leader>v <C-^>

" Delete buffer, preserving layout if not quickfix list
nnoremap <Leader>q :Bd<CR>
autocmd BufReadPost quickfix nnoremap <buffer> <Leader>q :bd<CR>

" Add newlines from normal mode
nnoremap <CR> o<Esc>k
nnoremap <S-CR> O<Esc>

"" Don't override the default <CR> bind in the quickfix list
autocmd BufReadPost quickfix nnoremap <buffer> <CR> <CR>

" Remove highlight
nnoremap <Leader>h :noh<CR>

" Fuzzy find filenames
nnoremap <Leader>p :GFiles<CR>
nnoremap <Leader>P :Files<CR>

" Shorthand edit in directory of open buffer
nnoremap <Leader>l :e %:h/

" Move fzf results into quickfix
let $FZF_DEFAULT_OPTS = '--bind ctrl-a:select-all'

" :grep motions and shortcut
nmap gs <plug>(GrepperOperator)
xmap gs <plug>(GrepperOperator)
nnoremap <Leader>/ :grep 

" Fuzzy find text
nnoremap <Leader>f :Lines<CR>
nnoremap <Leader>F :Rg<CR>

"" Customised fzf.vim Rg implementation to ignore lockfiles
command! -bang -nargs=* Rg
  \ call fzf#vim#grep(
  \   'rg -g "!*.lock" --column --line-number --no-heading --color=always --smart-case '.shellescape(<q-args>), 1,
  \   <bang>0 ? fzf#vim#with_preview('up:60%')
  \           : fzf#vim#with_preview('right:50%:hidden', '?'),
  \   <bang>0)

" Async make
nnoremap <Leader>m :Make 

" Navigate location list
nmap <silent> <Leader>j :lopen<CR>
nmap <silent> <C-k> :labove<CR>
nmap <silent> <C-j> :lbelow<CR>

" Navigate quickfix list
nmap <silent> <Leader>J :copen<CR>
nmap <silent> <C-h> :cprev<CR>
nmap <silent> <C-l> :cnext<CR>

" LSP gotos
nmap <silent> gd <Cmd>lua vim.lsp.buf.definition()<CR>
nmap <silent> gr <Cmd>lua vim.lsp.buf.references()<CR>

" Rename LSP symbol
nmap <Leader>r <Cmd>lua vim.lsp.buf.rename()<CR>

" Symbols search
nnoremap <silent> <Leader>s <Cmd>lua vim.lsp.buf.document_symbol()<CR>
nnoremap <silent> <Leader>S <Cmd>lua vim.lsp.buf.workspace_symbol()<CR>

" Trigger LSP completions and close on selection
inoremap <silent><expr> <C-space> compe#complete()
inoremap <silent><expr> <CR>      compe#confirm('<CR>')

" Format/fix active buffer
nnoremap <silent> <Leader>z :ALEFix<CR>

" Show documentation (type info) in preview window
nnoremap <silent> <Leader>d <Cmd>lua vim.lsp.buf.hover()<CR>

" Show diagnostics in popup
nnoremap <silent> <Leader>e <Cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>

" Offer code actions
nnoremap <silent> <Leader>g <Cmd>lua vim.lsp.buf.code_action()<CR>

" Digraphs
dig NN 8469  " ℕ
dig fi 8718  " ∎
dig .- 8760  " ∸
dig (< 10216 " ⟨
dig >) 10217 " ⟩


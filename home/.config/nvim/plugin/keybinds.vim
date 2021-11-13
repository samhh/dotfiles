let mapleader = ' '

" Disable unneeded Ex mode bind that's easy to mistakenly hit
nnoremap Q <NOP>

" Remap a questionable default for more consistency with C and D
nnoremap Y y$

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

" Non-omni completion
imap <silent> <C-d> <C-n>

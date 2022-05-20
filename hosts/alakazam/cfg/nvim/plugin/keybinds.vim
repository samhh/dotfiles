let mapleader = ' '

" Separate, ergonomic, mnenomic cut and delete
nnoremap d "_d
nnoremap D "_D
nnoremap c "_c
nnoremap C "_C
nnoremap x d
nnoremap xx dd
nnoremap X D

nnoremap Q <NOP>

" Easier redo
nnoremap U <C-r>

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
nmap gp <plug>(GrepperOperator)
nnoremap <Leader>/ :silent grep ''<Left>

" Async make
nnoremap <Leader>m :Make 

" Navigate location list
nmap <silent> gL :lprev<CR>
nmap <silent> gl :lnext<CR>

" Navigate quickfix list
nmap <silent> gF :cprev<CR>
nmap <silent> gf :cnext<CR>

" Navigate jumplist
nmap <silent> gJ <C-o>
nmap <silent> gj <C-i>

" Non-omni completion
imap <silent> <C-d> <C-n>

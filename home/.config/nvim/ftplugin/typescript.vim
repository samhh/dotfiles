setlocal foldenable
setlocal foldmethod=indent
setlocal makeprg=yarn
let g:test#javascript#jest#file_pattern = '\v(tests?\/.+|\.test\.)(j|t)sx?$'
let b:ale_fixers = ['prettier']

func Eatchar(pat)
    let c = nr2char(getchar(0))
    return (c =~ a:pat) ? '' : c
endfunc

iabbrev <silent> describe$ describe('', () => {<CR><Esc>k_f'a<C-R>=Eatchar('\s')<Esc>
iabbrev <silent> test$ test('', () => {<CR><Esc>k_f'a<C-R>=Eatchar('\s')<Esc>
iabbrev <silent> it$ it('', () => {<CR><Esc>k_f'a<C-R>=Eatchar('\s')<Esc>
iabbrev <silent> pbt$ fc.assert(fc.property(<CR><C-R>=Eatchar('\s')<Esc>


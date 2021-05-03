setlocal foldmethod=expr
let g:ale_javascript_eslint_use_global = 1
let g:ale_javascript_eslint_executable = 'eslint_d'
let b:ale_linters = ['eslint']
let b:ale_fixers = ['prettier']

func Eatchar(pat)
    let c = nr2char(getchar(0))
    return (c =~ a:pat) ? '' : c
endfunc

iabbrev <silent> desc$ describe('', () => {<CR><Esc>k_f'a<C-R>=Eatchar('\s')<Esc>
iabbrev <silent> test$ test('', () => {<CR><Esc>k_f'a<C-R>=Eatchar('\s')<Esc>
iabbrev <silent> it$ it('', () => {<CR><Esc>k_f'a<C-R>=Eatchar('\s')<Esc>
iabbrev <silent> pbt$ fc.assert(fc.property(<CR><C-R>=Eatchar('\s')<Esc>
iabbrev <silent> pbta$ fc.assert(fc.asyncProperty(<CR><C-R>=Eatchar('\s')<Esc>


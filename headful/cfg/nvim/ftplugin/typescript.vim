setlocal foldmethod=expr

func Eatchar(pat)
    let c = nr2char(getchar(0))
    return (c =~ a:pat) ? '' : c
endfunc

iabbrev <silent> desc$ describe('', () => {<CR><Esc>k_f'a<C-R>=Eatchar('\s')<Esc>
iabbrev <silent> it$ it('', () => {<CR><Esc>k_f'a<C-R>=Eatchar('\s')<Esc>
iabbrev <silent> pbt$ fc.assert(fc.property(<CR><C-R>=Eatchar('\s')<Esc>
iabbrev <silent> pbta$ fc.assert(fc.asyncProperty(<CR><C-R>=Eatchar('\s')<Esc>

iabbrev <silent> esld$ /* eslint-disable */<Left><Left><Left>
iabbrev <silent> esle$ /* eslint-enable */<Left><Left><Left>
iabbrev <silent> esll$ // eslint-disable-line
iabbrev <silent> esln$ // eslint-disable-next-line

" Patch TypeScript support in vim-matchup. See:
" https://github.com/andymass/vim-matchup/issues/194#issuecomment-968132731
let b:match_ignorecase = 0
let b:match_words =
    \'<:>,' .
    \'<\@<=\([^ \t>/]\+\)\%(\s\+[^>]*\%([^/]>\|$\)\|>\|$\):<\@<=/\1>,' .
    \'<\@<=\%([^ \t>/]\+\)\%(\s\+[^/>]*\|$\):/>'

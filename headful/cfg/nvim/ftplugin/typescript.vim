setlocal foldmethod=expr

" Patch TypeScript support in vim-matchup. See:
" https://github.com/andymass/vim-matchup/issues/194#issuecomment-968132731
let b:match_ignorecase = 0
let b:match_words =
    \'<:>,' .
    \'<\@<=\([^ \t>/]\+\)\%(\s\+[^>]*\%([^/]>\|$\)\|>\|$\):<\@<=/\1>,' .
    \'<\@<=\%([^ \t>/]\+\)\%(\s\+[^/>]*\|$\):/>'

let current_compiler = "cabal_build"

CompilerSet makeprg=cabal\ build

CompilerSet efm =%W%f:%l:%c:\ Warning:%m
CompilerSet efm+=%W%f:%l:%c:\ Warning:
CompilerSet efm+=%E%f:%l:%c:%m
CompilerSet efm+=%E%f:%l:%c:
CompilerSet efm+=%C\ \ %#%m
CompilerSet efm+=%-G%.%#
CompilerSet efm+=%-G%.%#


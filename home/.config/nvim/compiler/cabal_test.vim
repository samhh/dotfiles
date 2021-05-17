let current_compiler = "cabal_test"

CompilerSet makeprg=cabal\ test

"" In actuality this first line will always end with `error: %m` where the %m
"" is optional, and with two %E one of the two possibilities will break
"" depending upon their order... /shrug
CompilerSet efm =%E%f:%l:%c:\ %m
CompilerSet efm+=%Z\ %*[\ ]\|\ %*[\ ]^^%#
CompilerSet efm+=%-G%[0-9]+\ \|%.%#
CompilerSet efm+=%-G\ %*[\ ]\|
CompilerSet efm+=%C\ %*[\ ]•\ %m
CompilerSet efm+=%C%m
CompilerSet efm+=%-G%.%#


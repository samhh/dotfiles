let current_compiler = "jest"

CompilerSet makeprg=./node_modules/.bin/jest\ --no-colors\ --collectCoverage=false

CompilerSet efm =%E\ \ ●\ %m
CompilerSet efm+=%Z\ %\\{4}%.%#Error:\ %f:\ %m\ (%l:%c):%\\=
CompilerSet efm+=%Z\ %\\{6}at\ %\\S%#\ (%f:%l:%c),
CompilerSet efm+=%+C\ %\\{4}%\\w%.%#
CompilerSet efm+=%+C\ %\\{4}%[-+]%.%#
CompilerSet efm+=%-C%.%#
CompilerSet efm+=%-G%.%#


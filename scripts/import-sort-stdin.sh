#!/bin/sh

code="$(cat /dev/stdin)"
path="$1"

# import-sort uses the file extension to determine how to format. It uses other
# dependencies in node_modules/ too so our proxy file needs to be in the same
# directory.
ext=$(echo "$path" | sed 's/^.*\.//')
proxy="$path.import-sort.tmp.$ext"

# import-sort doesn't print to stdout if there's nothing to do, so using --write
# on our proxy file which if nothing else contains unmodified code avoids some
# awkward conditionals.
echo "$code" > "$proxy"
./node_modules/.bin/import-sort --write "$proxy"
cat "$proxy"
rm "$proxy"

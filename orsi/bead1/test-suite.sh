#!/bin/sh
./a.out ./tesztek/$1/input.txt > /dev/null
git diff --no-index --color-words ./tesztek/$1/output.txt ./output.txt
rm ./output.txt


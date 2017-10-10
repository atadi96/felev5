#!/bin/sh
./a.out ./tesztek/$1/input.txt > out.tmp
git diff --color-words --no-index ./tesztek/$1/output.txt ./out.tmp
rm ./out.tmp


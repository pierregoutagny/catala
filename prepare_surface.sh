#!/bin/sh

[[ $# -ne 1 ]] && echo "needs a --conc-optim=generate-surface output file" && exit

file=$1

echo '```catala'
sed '/Concolic interpreter done/q' $file | grep -v RESULT
echo '```'


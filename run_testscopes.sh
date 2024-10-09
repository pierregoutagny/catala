#!/bin/sh

[[ $# -ne 1 ]] && echo "error: expects filename as argument" >&2 && exit 1

file=$1

get_scopes ()
{
    sed -Ene 's/^.*(Test[0-9]+):$/\1/gp' "$1" | uniq
}

for scope in $(get_scopes $file)
do
    printf "$scope..."
    r=$(catala Interpret --disable-warnings --optimize -s $scope $file)
    echo "$r" | grep -q "Computation successful" && printf "ok\n" || printf "bad\n"
done


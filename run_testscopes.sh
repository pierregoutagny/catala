#!/bin/bash

[[ $# -ne 2 ]] && echo "error: expects filename and scope as argument" >&2 && exit 1

file=$1
scope=$2

get_scopes ()
{
    sed -Ene "s/^.*(Test_${scope}_[0-9]+):$/\1/gp" "$1" | uniq
}

for scope in $(get_scopes $file)
do
    printf "$scope..."
    r=$(catala Interpret --disable-warnings --optimize -s $scope $file)
    echo "$r" | grep -q "Computation successful" && printf "ok\n" || printf "bad\n"
done


#!/usr/bin/env bash
read -p "Which file types do you want to backup " file_suffix
read -p "Which directory do you want to backup to " dir_name

test -d $PWD/$dir_name || mkdir -m 700 $PWD/$dir_name

find $PWD -path $PWD/$dir_name -prune -o \
  -name "*$file_suffix" -exec cp {} $PWD/$dir_name/ \;
exit 0
#!/bin/bash

file_path="$1"

dir_path="${file_path%/*}"
[[ "$file_path" == "$dir_path" ]] && dir_path="."

filename="${file_path##*/}"
name="${filename%.*}"

obj_file="$dir_path/$name.o"
asm_file="$dir_path/$name.asm"
exe_file="$dir_path/$name"

runghc Main.hs "$file_path" --compiled && \
nasm -f elf32 -o "$obj_file" "$asm_file" && \
gcc -m32 -no-pie -nostartfiles -o "$exe_file" "$obj_file" -e _start && \
{ [ "$2" = "-save-temps" ] || rm "$obj_file" "$asm_file"; }
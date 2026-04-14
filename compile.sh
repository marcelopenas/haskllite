#!/bin/bash

file_path="$1"
filename="${file_path##*/}"
name="${filename%.*}"

obj_file="$name.o"
asm_file="$name.asm"

runghc main.hs "$file_path" && \
nasm -f elf32 -o "$obj_file" "$asm_file" && \
gcc -m32 -no-pie -nostartfiles -o "$name" "$name".o -e _start

# Cleanup
if [ "$2" != "-save-temps" ]; then
    rm "$obj_file" "$asm_file"
fi

#!/bin/bash

find . -maxdepth 1 -type f ! -name "*.*" -delete
find test -maxdepth 1 -type f ! -name "*.*" -delete
rm ./*.o ./*.asm
rm ./test/*.o ./test/*.asm

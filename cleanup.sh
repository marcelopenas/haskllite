#!/bin/bash

rm ./*.o ./*.asm
find . -maxdepth 1 -type f ! -name "*.*" -delete

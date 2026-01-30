#!/usr/bin/fish
odin run . -- $argv[1]
nasm ./output.nasm -f elf64
ld ./output.o -o output

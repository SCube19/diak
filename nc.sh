#!/bin/bash
nasm -f elf64 -g -F dwarf -o ./diakrytynizator.o ./diakrytynizator.asm
ld -o ./diakrytynizator ./diakrytynizator.o
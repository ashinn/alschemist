BITS 32
section .text
foo:
shld bx, ax, 9
shrd [ebx], ax, 9
shld ebx, eax, 9
shrd [ebx], eax, 9
shld bx, ax, cl
shrd [ebx], ax, cl
shld ebx, eax, cl
shrd [ebx], eax, cl

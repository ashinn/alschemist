BITS 32
section .text
foo:
fbld tword [eax]
fbstp tword [eax]
fstcw word [ebx]
fldcw word [ebx]
fnstcw word [ebx]
fstenv [eax]
fnstenv [ebx]
ffree st2
fldenv [edx]
fsave [edx]
fnsave [edx]
frstor [edx]
fxsave [edx]
fxrstor [edx]
fstsw ax
fstsw word [ebx]
fnstsw ax
fnstsw word [ebx]

BITS 16
section .text
foo:
jo near dword foo
jno near word foo
jb 0
jc near dword foo
jnae near word foo
jnb 0
jnc near dword foo
jae near word foo
je 0
jz near dword foo
jne near word foo
jnz 0
jbe near dword foo
jna near word foo
ja 0
jnbe near dword foo
js near word foo
jns 0
jp near dword foo
jpe near word foo
jnp 0
jpo near dword foo
jl near word foo
jnge 0
jge near dword foo
jnl near word foo
jle 0
jng near dword foo
jnle near word foo
jg 0

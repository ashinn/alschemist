BITS 32
section .text
foo:
rep 
insd
rep 
outsw
rep 
lodsb
rep 
stosd
rep 
movsb
repe 
cmpsb
repz 
cmpsd
repne 
scasd
repnz 
scasb
lock 
add byte [eax], 1
lock 
dec dword [edx]
lock 
xor [ecx], ecx

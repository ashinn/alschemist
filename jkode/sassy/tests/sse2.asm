BITS 32
section .text
foo:
movapd xmm0, xmm1
movupd xmm2, [edx]
movdqa [eax], xmm3
movdqu xmm0, xmm1
movhpd xmm0, [ecx]
movmskpd ebx, xmm4
movlpd [ecx], xmm0
movsd xmm5, xmm6
movsd xmm7, [esi]
movsd [esi], xmm7
addpd xmm0, xmm1
subpd xmm2, [ecx]
mulpd xmm0, xmm1
divpd xmm2, [ecx]
sqrtpd xmm0, xmm1
maxpd xmm2, [ecx]
minpd xmm0, xmm1
andpd xmm2, [ecx]
andnpd xmm0, xmm1
orpd xmm2, [ecx]
xorpd xmm0, xmm1
unpckhpd xmm2, [ecx]
unpcklpd xmm0, xmm1
cvtpd2dq xmm2, [ecx]
cvttpd2dq xmm0, xmm1
cvtdq2ps xmm2, [ecx]
cvtps2dq xmm0, xmm1
cvttps2dq xmm2, [ecx]
cvtpd2ps xmm0, xmm1
punpckhqdq xmm2, [ecx]
punpcklqdq xmm0, xmm1
addsd xmm2, xmm3
subsd xmm4, [edi]
mulsd xmm2, xmm3
divsd xmm4, [edi]
maxsd xmm2, xmm3
minsd xmm4, [edi]
sqrtsd xmm2, xmm3
comisd xmm4, [edi]
ucomisd xmm2, xmm3
cvtdq2pd xmm4, [edi]
cvtps2pd xmm2, xmm3
cvtsd2ss xmm4, [edi]
cmppd xmm0, xmm1, 10
shufpd xmm2, [ebx], 20
pshuflw xmm0, xmm1, 10
pshufhw xmm2, [ebx], 20
pshufd xmm0, xmm1, 10
cmpsd xmm4, xmm5, 20
cmpsd xmm6, [eax], 30
cvttpd2pi mm0, xmm1
cvtpd2pi mm0, [ebp]
pause
lfence
mfence
clflush [ebx]
pmuludq mm0, mm1
paddq mm0, [edx]
psubq xmm3, xmm1
pmuludq xmm3, [edx]
maskmovdqu xmm1, xmm2
movnti [edx], eax
movq2dq xmm3, mm4
movdq2q mm5, xmm7
movntpd [eax], xmm3
movntdq [ebx], xmm4
pslldq xmm3, 20
psrldq xmm3, 20
cvtpi2pd xmm3, mm4
cvtpi2pd xmm3, [ecx]
cvtss2sd xmm3, xmm6
cvtss2sd xmm3, [edx]
cvtsd2si ecx, xmm3
cvttsd2si edx, [edx]
cvtsi2sd xmm3, eax
cvtsi2sd xmm3, [eax]

BITS 32
section .text
foo:
fcmovb st0, st2
fcmove st0, st3
fcmovbe st0, st4
fcmovu st0, st5
fcmovnb st0, st6
fcmovne st0, st7
fcmovnbe st0, st1
fcmovnu st0, st2
fxch
fucom st3
fld tword [eax]
fstp qword [ebx]
fld dword [ecx]
fstp st4
fst dword [edx]
fst qword [ebx]
fst st3
fild word [ebx]
fistp dword [ebx]
fild qword [ebx]
fist word [ecx]
ficom dword [ecx]
ficomp word [ecx]
fcomp dword [edi]
fcom qword [edi]
fcomp st0
fcomi st0, st7
fcomip st0, st6
fucomi st0, st5
fucomip st0, st4

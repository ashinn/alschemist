(text
 (begin (nop)
	(nop)
	(seq (nop)
	     (nop)
	     z!
	     (nop)
	     (inv z!)
	     (nop))
	(nop)))

; 00000000  90                nop
; 00000001  90                nop
; 00000002  90                nop
; 00000003  90                nop
; 00000004  7504              jnz 0xa
; 00000006  90                nop
; 00000007  7401              jz 0xa
; 00000009  90                nop
; 0000000A  90                nop

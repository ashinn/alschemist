(
(bt  si si)
(btc (& 100 200 edx) si)
(btr (& (* 8 eax)) edi)
(bts edi edi)
(bt  si 9)
(btc (word (& 100 200 edx)) (byte 9))
(btr edi 9)
(bts (dword (& (* 8 eax))) (byte 9))
)

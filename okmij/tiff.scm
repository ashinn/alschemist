;****************************************************************************
;
;                     Tag Image File Format (TIFF)
;
;
; Tiff tag definitions were borrowed from:
;      > Copyright (c) 1988, 1990 by Sam Leffler.
;      > All rights reserved.
;      >
;      > This file is provided for unrestricted use provided that this
;      > legend is included on all tape media and as a part of the
;      > software program in whole or part.  Users may copy, modify or
;      > distribute this file at will.
;      >
;      > Based on Rev 5.0 from:
;      > Developer's Desk              Window Marketing Group
;      > Aldus Corporation             Microsoft Corporation
;      > 411 First Ave. South          16011 NE 36th Way
;      > Suite 200                     Box 97017
;      > Seattle, WA  98104            Redmond, WA  98073-9717
;      > 206-622-5500                  206-882-8080
;
; Now updated for TIFF 6
; http://www.wotsit.org/download.asp?f=tiff6
;
; We rely on an ENDIAN-PORT
; A port with the following operations
;   endian-port-set-bigendian!::   PORT -> UNSPECIFIED
;   endian-port-set-littlendian!:: PORT -> UNSPECIFIED
;   endian-port-read-int1:: PORT -> UINTEGER (byte)
;   endian-port-read-int2:: PORT -> UINTEGER
;   endian-port-read-int4:: PORT -> UINTEGER
;   endian-port-setpos PORT INTEGER -> UNSPECIFIED
;
; Also needed SRFIs: SRFI-4 (uniform vectors), SRFI-9 (records)
; Actually, we're using structures, which can be translated to SRFI-9
; records.
;
; $Id: tiff.scm,v 2.0 2003/09/29 20:05:12 oleg Exp oleg $


;------------------------------------------------------------------------
;                              TIFF file header
; It is always written at the very beginning of the TIFF file

;  unsigned short magic;         // magic number (defines byte order)
;  unsigned short version;       // TIFF version number
;  unsigned long  diroffset;     // byte offset to the first directory

(define TIFF:BIGENDIAN-magic    #x4d4d)		; 'MM'
(define TIFF:LITTLEENDIAN-magic #x4949)		; 'II'
(define TIFF:VERSION 42)

; procedure: TIFF:read-header ENDIAN-PORT -> OFFSET
;
; Reads and checks the TIFF header, sets the reader to
; the appropriate little/big endian mode and returns the byte
; offset to the first TIFF directory
(define (TIFF:read-header eport)
  (let ((magic-word (endian-port-read-int2 eport)))
    (cond
      ((= magic-word TIFF:BIGENDIAN-magic) 
       (endian-port-set-bigendian! eport))
      ((= magic-word TIFF:LITTLEENDIAN-magic)
       (endian-port-set-littlendian! eport))
      (else (error "invalid magic word 0x" (number->string magic-word 16)
     	       "of the TIFF file"))))
  (let ((version (endian-port-read-int2 eport)))
    (if (not (= version TIFF:VERSION))
      (error "TIFF file version " version "differs from the standard "
      	    TIFF:VERSION)))
  (endian-port-read-int4 eport))

;------------------------------------------------------------------------
; 		    TIFF tags: codes and values
;
; A tag dictionary, tagdict, structure helps translate between
; tag-symbols and their numerical values.
;
; procedure: tagdict-get-by-name TAGDICT TAG-NAME -> INT
; where TAG-NAME is a symbol. 
; Translate a symbolic representation of a TIFF tag into a numeric
; representation.
; An error is raised if the lookup fails.
;
; procedure: tagdict-get-by-num TAGDICT INT -> TAG-NAME or #f
; Translate from a numeric tag value to a symbolic representation,
; if it exists. Return #f otherwise.
;
; procedure: tagdict-tagval-get-by-name TAGDICT TAG-NAME VAL-NAME -> INT
; where VAL-NAME is a symbol.
; Translate from the symbolic representation of a value associated
; with TAG-NAME in the TIFF directory, into the numeric representation.
; An error is raised if the lookup fails.
;
; procedure: tagdict-tagval-get-by-num TAGDICT TAG-NAME INT -> VAL-NAME or #f
; Translate from a numeric value associated with TAG-NAME in the TIFF
; directory to a symbolic representation, if it exists. Return #f
; otherwise.
;
; procedure: make-tagdict ((TAG-NAME INT (VAL-NAME . INT) ...) ...)
; Build a tag dictionary
;
; procedure: tagdict? TAGDICT -> BOOL
; 
; procedure: tagdict-add-all DEST-DICT SRC-DICT -> DEST-DICT
; Join two dictionaries
;
; The variable tiff-standard-tagdict is initialized to the dictionary
; of standard TIFF tags.

; Usage scenario:
;    (tagdict-get-by-name  tiff-standard-tagdict 'TIFFTAG:IMAGEWIDTH) => 256
;    (tagdict-get-by-num   tiff-standard-tagdict 256) => 'TIFFTAG:IMAGEWIDTH
;    (tagdict-tagval-get-by-name tiff-standard-tagdict
;       'TIFFTAG:COMPRESSION 'LZW) => 5
;    (tagdict-tagval-get-by-num  tiff-standard-tagdict
;       'TIFFTAG:COMPRESSION 5) => 'LZW
;
;    (define extended-tagdict
;       (tagdict-add-all tiff-standard-tagdict
;          (make-tagdict
;		'((WAupper_left_lat 33004)
;		  (WAhemisphere 33003 (North . 1) (South . 2))))))

(define-structure p-tiff-tag-dict table)


(define tagdict? p-tiff-tag-dict?)

(define (make-tagdict args)
  (for-each				; error-check each dict association
    (lambda (arg)
      (or
	(and
	  (pair? arg)
	  (list? arg)
	  (symbol? (car arg))
	  (integer? (cadr arg)))
	(error "make-tagdict: bad association to add: " arg))
      (for-each
	(lambda (val-assoc)
	  (or
	    (and 
	      (pair? val-assoc)
	      (symbol? (car val-assoc))
	      (integer? (cdr val-assoc)))
	    (error "make-tagdict: bad tag value association: " val-assoc)))
	(cddr arg)))
    args)
  (make-p-tiff-tag-dict args))


; procedure: tagdict-add-all DEST-DICT SRC-DICT -> DEST-DICT
; Join two dictionaries
(define (tagdict-add-all dest-dict src-dict)
  (assert (tagdict? dest-dict) (tagdict? src-dict))
  (make-p-tiff-tag-dict
    (append (p-tiff-tag-dict-table dest-dict)
            (p-tiff-tag-dict-table src-dict))))

; procedure: tagdict-get-by-name TAGDICT TAG-NAME -> INT
; An error is raised if the lookup fails.

(define (tagdict-get-by-name dict tag-name)
  (assert (tagdict? dict))
  (cond
    ((assq tag-name (p-tiff-tag-dict-table dict))
      => cadr)
    (else
      (error "tagdict-get-by-name: can't translate: " tag-name))))


; procedure: tagdict-get-by-num TAGDICT INT -> TAG-NAME or #f
(define (tagdict-get-by-num dict tag-int)
  (assert (tagdict? dict))
  (any? 
    (lambda (table-row)
      (and (= (cadr table-row) tag-int) (car table-row)))
    (p-tiff-tag-dict-table dict)))


; procedure: tagdict-tagval-get-by-name TAGDICT TAG-NAME VAL-NAME -> INT
; An error is raised if the lookup fails.
(define (tagdict-tagval-get-by-name dict tag-name val-name)
  (assert (tagdict? dict))
  (cond
    ((assq tag-name (p-tiff-tag-dict-table dict))
      => (lambda (table-row)
	   (cond
	     ((assq val-name (cddr table-row)) => cdr)
	     (else
	       (error "tagdict-tagval-get-by-name: can't translate "
		 tag-name val-name)))))
    (else
      (error "tagdict-tagval-get-by-name: unknown tag: " tag-name))))


; procedure: tagdict-tagval-get-by-num TAGDICT TAG-NAME INT -> VAL-NAME or #f
; Translate from a numeric value associated with TAG-NAME in the
; TIFF directory.
(define (tagdict-tagval-get-by-num dict tag-name val-int)
  (assert (tagdict? dict))
  (cond
    ((assq tag-name (p-tiff-tag-dict-table dict))
      => (lambda (table-row)
	   (any? 
	     (lambda (assc)
	       (and (= val-int (cdr assc)) (car assc)))
	     (cddr table-row))))
    (else
      (error "tagdict-tagval-get-by-num: unknown tag: " tag-name))))
 
(define tiff-standard-tagdict
  (make-tagdict
    '(
       (TIFFTAG:SUBFILETYPE 254	        ; subfile data descriptor
	 (TIFFTAG:REDUCEDIMAGE . #x1)	  ; reduced resolution version
	 (TIFFTAG:PAGE . #x2)		  ; one page of many
	 (TIFFTAG:MASK . #x4))
       (TIFFTAG:OSUBFILETYPE 255	; +kind of data in subfile
         (TIFFTAG:IMAGE . 1)		  ; full resolution image data
	 (TIFFTAG:REDUCEDIMAGE . 2)	  ; reduced size image data
	 (TIFFTAG:PAGE . 3))		  ; one page of many
       (TIFFTAG:IMAGEWIDTH 256)	        ; image width in pixels
       (TIFFTAG:IMAGELENGTH 257)	; image height in pixels
       (TIFFTAG:BITSPERSAMPLE 258)	; bits per channel (sample)
        
       (TIFFTAG:COMPRESSION 259	        ; data compression technique
	 (NONE . 1)			  ; dump mode
	 (CCITTRLE . 2)		          ; CCITT modified Huffman RLE
	 (CCITTFAX3 . 3)		  ; CCITT Group 3 fax encoding
	 (CCITTFAX4 . 4)		  ; CCITT Group 4 fax encoding
	 (LZW . 5)			  ; Lempel-Ziv  & Welch
	 (NEXT . 32766)		          ; NeXT 2-bit RLE
	 (CCITTRLEW . 32771)		  ; #1 w/ word alignment
	 (PACKBITS . 32773)		  ; Macintosh RLE
	 (THUNDERSCAN . 32809)		  ; ThunderScan RLE
	 (PICIO . 32900)		  ; old Pixar picio RLE
	 (SGIRLE . 32901))		  ; Silicon Graphics RLE

       (TIFFTAG:PHOTOMETRIC 262	        ; photometric interpretation
	 (MINISWHITE . 0)		  ; min value is white
	 (MINISBLACK . 1)		  ; min value is black
	 (RGB . 2)			  ; RGB color model
	 (PALETTE . 3)			  ; color map indexed
	 (MASK . 4)			  ; holdout mask
	 (DEPTH . 32768))		  ; z-depth data

       (TIFFTAG:THRESHOLDING 263	; +thresholding used on data
	 (BILEVEL . 1)			  ; b&w art scan
	 (HALFTONE . 2)		          ; or dithered scan
	 (ERRORDIFFUSE . 3))		  ; usually floyd-steinberg

       (TIFFTAG:CELLWIDTH 264)		; +dithering matrix width
       (TIFFTAG:CELLLENGTH 265)	        ; +dithering matrix height
       (TIFFTAG:FILLORDER 266		; +data order within a byte
	 (MSB2LSB . 1)			  ; most significant -> least
	 (LSB2MSB . 2))		          ; least significant -> most

       (TIFFTAG:DOCUMENTNAME 269)	; name of doc. image is from
       (TIFFTAG:IMAGEDESCRIPTION 270)	; info about image
       (TIFFTAG:MAKE 271)		; scanner manufacturer name
       (TIFFTAG:MODEL 272)		; scanner model name/number
       (TIFFTAG:STRIPOFFSETS 273)	; offsets to data strips

       (TIFFTAG:ORIENTATION 274	        ; +image orientation
	 (TOPLEFT . 1)			  ; row 0 top, col 0 lhs
	 (TOPRIGHT . 2)		          ; row 0 top, col 0 rhs
	 (BOTRIGHT . 3)		          ; row 0 bottom, col 0 rhs
	 (BOTLEFT . 4)			  ; row 0 bottom, col 0 lhs
	 (LEFTTOP . 5)			  ; row 0 lhs, col 0 top
	 (RIGHTTOP . 6)		          ; row 0 rhs, col 0 top
	 (RIGHTBOT . 7)		          ; row 0 rhs, col 0 bottom
	 (LEFTBOT . 8))		          ; row 0 lhs, col 0 bottom

       (TIFFTAG:SAMPLESPERPIXEL 277)	; samples per pixel
       (TIFFTAG:ROWSPERSTRIP 278)	; rows per strip of data
       (TIFFTAG:STRIPBYTECOUNTS 279)	; bytes counts for strips
       (TIFFTAG:MINSAMPLEVALUE 280)	; +minimum sample value
       (TIFFTAG:MAXSAMPLEVALUE 281)	; maximum sample value
       (TIFFTAG:XRESOLUTION 282)	; pixels/resolution in x
       (TIFFTAG:YRESOLUTION 283)	; pixels/resolution in y

       (TIFFTAG:PLANARCONFIG 284	; storage organization
	 (CONTIG . 1)			  ; single image plane
	 (SEPARATE . 2))		  ; separate planes of data

       (TIFFTAG:PAGENAME 285)		; page name image is from
       (TIFFTAG:XPOSITION 286)		; x page offset of image lhs
       (TIFFTAG:YPOSITION 287)		; y page offset of image lhs
       (TIFFTAG:FREEOFFSETS 288)	; +byte offset to free block
       (TIFFTAG:FREEBYTECOUNTS 289)	; +sizes of free blocks

       (TIFFTAG:GRAYRESPONSEUNIT 290	; gray scale curve accuracy
	 (S10 . 1)			  ; tenths of a unit
	 (S100 . 2)			  ; hundredths of a unit
	 (S1000 . 3)			  ; thousandths of a unit
	 (S10000 . 4)			  ; ten-thousandths of a unit
	 (S100000 . 5))		          ; hundred-thousandths
       (TIFFTAG:GRAYRESPONSECURVE 291)	; gray scale response curve

       (TIFFTAG:GROUP3OPTIONS 292	; 32 flag bits
	 (ENCODING2D . #x1)		  ; 2-dimensional coding
	 (UNCOMPRESSED . #x2)		  ; data not compressed
	 (FILLBITS . #x4))		  ; fill to byte boundary
       (TIFFTAG:GROUP4OPTIONS 293	; 32 flag bits
	 (UNCOMPRESSED . #x2))		; data not compressed

       (TIFFTAG:RESOLUTIONUNIT 296	; units of resolutions
	 (NONE . 1)			  ; no meaningful units
	 (INCH . 2)			  ; english
	 (CENTIMETER . 3))		  ; metric

       (TIFFTAG:PAGENUMBER 297)	        ; page numbers of multi-page

       (TIFFTAG:COLORRESPONSEUNIT 300	; color scale curve accuracy
	 (S10 . 1)			  ; tenths of a unit
	 (S100 . 2)			  ; hundredths of a unit
	 (S1000 . 3)			  ; thousandths of a unit
	 (S10000 . 4)			  ; ten-thousandths of a unit
	 (S100000 . 5))			  ; hundred-thousandths

       (TIFFTAG:COLORRESPONSECURVE 301); RGB response curve
       (TIFFTAG:SOFTWARE 305)		; name & release
       (TIFFTAG:DATETIME 306)		; creation date and time
       (TIFFTAG:ARTIST 315)		; creator of image
       (TIFFTAG:HOSTCOMPUTER 316)	; machine where created
       (TIFFTAG:PREDICTOR 317)		; prediction scheme w/ LZW
       (TIFFTAG:WHITEPOINT  318)	; image white point
       (TIFFTAG:PRIMARYCHROMATICITIES 319) ; primary chromaticities
       (TIFFTAG:COLORMAP 320)		; RGB map for pallette image
       (TIFFTAG:BADFAXLINES 326)	; lines w/ wrong pixel count

       (TIFFTAG:CLEANFAXDATA 327	; regenerated line info
	 (CLEAN . 0)			  ; no errors detected
	 (REGENERATED . 1)		  ; receiver regenerated lines
	 (UNCLEAN . 2))		          ; uncorrected errors exist
       
       (TIFFTAG:CONSECUTIVEBADFAXLINES 328); max consecutive bad lines

       (TIFFTAG:MATTEING 32995)	        ; alpha channel is present
)))

;------------------------------------------------------------------------
; 			TIFF directory entry
; a descriptor of a TIFF "item", which can be image data, document description,
; time stamp, etc, depending on the tag. Thus an entry has the following
; structure:
;  unsigned short tag;
;  unsigned short type;          // data type: byte, short word, etc.
;  unsigned long  count;         // number of items; length in spec
;  unsigned long  val_offset;    // byte offset to field data
;
; The values associated with each entry are disjoint and may appear anywhere
; in the file (so long as they are placed on a word boundary).
;
; Note, If the value takes 4 bytes or less, then it is placed in the offset
; field to save space.  If the value takes less than 4 bytes, it is
; *left*-justified in the offset field.
; Note, that it's always *left* justified (stored in the lower bytes)
; no matter what the byte order (big- or little- endian) is!
; Here's the precise quote from the TIFF 6.0 specification:
; "To save time and space the Value Offset contains the Value instead of
; pointing to the Value if and only if the Value fits into 4 bytes. If
; the Value is shorter than 4 bytes, it is left-justified within the
; 4-byte Value Offset, i.e., stored in the lower- numbered
; bytes. Whether the Value fits within 4 bytes is determined by the Type
; and Count of the field."


; Could be easily implemented as a syntax-rule
(define-macro (make-define-env env-name . associations)
  `(begin
     (define ,env-name ',associations)
     ,@(map
	 (lambda (assc)			; (name val . other fields)
	   `(define ,(car assc) ,(cadr assc)))
	 associations)))

			; values of the 'data type' field
(make-define-env
  TIFF:types
  (TIFF:type-byte  1 "byte")	        ; 8-bit unsigned integer
  (TIFF:type-ascii 2 "ascii str")	; 8-bit bytes w/ last byte null
  (TIFF:type-short 3 "short")	        ; 16-bit unsigned integer
  (TIFF:type-long  4 "long")	        ; 32-bit unsigned integer
  (TIFF:type-rational  5 "rational")    ; 64-bit fractional (numer+denominator)
    				; The following was added in TIFF 6.0
  (TIFF:type-sbyte     6 "signed byte") ; 8-bit signed (2s-complement) integer
  (TIFF:type-undefined 7 "8-bit chunk") ; An 8-bit byte 
  (TIFF:type-sshort    8 "signed short"); 16-bit signed (2s-complement) integer
  (TIFF:type-slong     9 "signed long")	; 32-bit signed (2s-complement) integer
  (TIFF:type-srational 10 "signed rational")    ; two SLONGs (num+denominator)
  (TIFF:type-float     11 "IEEE 32-bit float")	; single precision (4-byte)
  (TIFF:type-double    12 "IEEE 64-bit double") ; double precision (8-byte)
)

(define-structure tiff-dir-entry tag type count val-offset value)

; procedure: TIFF:read-dir-entry EPORT -> TIFF-DIR-ENTRY
; 
; This procedure parses the current directory entry and
; returns a tiff-dir-entry structure. EPORT must point to the beginning
; of the entry in the TIFF directory. On exit, EPORT points to the
; next entry or the end of the directory. 
; TIFF-DIR-ENTRY contains all the data of the entry, plus a promise
; of entry's value. 
; The promise is forced only when the value is specifically requested
; by an object's user. That is, we won't rush to read and make the
; value (which may be pretty big: say the pixel matrix, etc).
;
; The promise closes over the EPORT!
;
; The value of an entry corresponds to its type: character, string,
; exact integer, floating-point number, or a uniform
; (u8, u16, or u32) vector. SRFI-4 is implied.

(define (TIFF:read-dir-entry eport)
  (let*
    ((tag        (endian-port-read-int2 eport))
     (type       (endian-port-read-int2 eport))
     (count      (endian-port-read-int4 eport))
      ; we read the val-offset later. We need to check the size and the type
      ; of the datum, because val-offset may contain the value itself,
      ; in its lower-numbered bytes, regardless of the big/little endian
      ; order!
    )
				; Some conversion procedures
    (define (u32->float x)		; unsigned 32-bit int -> IEEE float
      (error "u32->float is not yet implemented"))
    (define (u32->s32 x)		; unsigned 32-bit int -> signed 32 bit
      (if (>= x #x80000000)
	(- x #x100000000)
	x))
    ; (= (u32->s32 #x7fffffff) #x7fffffff)
    ; (= (u32->s32 #xffffffff) -1)

    (define (u16->s16 x)		; unsigned 16-bit int -> signed 16 bit
      (if (>= x #x8000)
	(- x #x10000) x))
    ; (= (u16->s16 32767) 32767)
    ; (= (u16->s16 32768) -32768)
    ; (= (u16->s16 65535) -1)

    (define (u8->s8 x)			; unsigned 8-bit int -> signed 8-bit
      (if (>= x #x80)
	(- x #x100) x))
    ; (= (u8->s8 127) 127)
    ; (= (u8->s8 128) -128)
    ; (= (u8->s8 255) -1)
		                        
    (define (read-double val-offset)
      (error "read-double: not yet implemented"))

            		; read an ascii string. Note, the last byte of
            		; an ascii string is always zero, which is
            		; included in 'count'
            		; but we don't need to read it
    (define (read-string val-offset)
      (assert (= TIFF:type-ascii type) (positive? count))
      (let ((str (make-string (- count 1))))
	(endian-port-setpos eport val-offset)
	(do ((i 0 (+ 1 i))) ((>= i (- count 1)) str)
	  (string-set! str i 
	    (ascii->char (endian-port-read-int1 eport))))))

               
               		; read an array of 'count' items
               		; return a *uniform* vector of read data:
			; u8vector, u16vector or u32vector
			; We roll-out the code for efficiency
    (define (read-array val-offset)
      (endian-port-setpos eport val-offset)
      (cond
	((or (= type TIFF:type-byte) (= type TIFF:type-undefined))
	  (let ((array (make-u8vector count)))
	    (do ((i 0 (+ 1 i))) ((>= i count) array)
	      (u8vector-set! array i (endian-port-read-int1 eport)))))
	((= type TIFF:type-short)
	  (let ((array (make-u16vector count)))
	    (do ((i 0 (+ 1 i))) ((>= i count) array)
	      (u16vector-set! array i (endian-port-read-int2 eport)))))
	((= type TIFF:type-long)
	  (let ((array (make-u32vector count)))
	    (do ((i 0 (+ 1 i))) ((>= i count) array)
	      (u32vector-set! array i (endian-port-read-int4 eport)))))
	(else (error "don't know how to read an array "
		"of type " type))))

    ; Now we need to figure out if val-offset contains the offset
    ; or the value (or a part of the value). If val-offset contains the
    ; value, we read it in val-offset and make the value a trivial promise
    ; (delay val-offset).
    ; If val-offset is an offset, then value is a non-trivial promise
    ; (which closes over EPORT).
    (assert (positive? count))
    (let*-values
      (((val-offset value)
	 (cond
	   ((> count 4) 		; for sure, val-offset is an offset
	     (let ((offset (endian-port-read-int4 eport)))
	       (if (= type TIFF:type-ascii) 
		 (values offset (delay (read-string offset)))
		 (values offset (delay (read-array offset))))))
	   ((> count 1)			; the iffy case
	     (cond
	       ((and (= count 2) (= type TIFF:type-short))
		 (let* ((v1 (endian-port-read-int2 eport))
			(v2 (endian-port-read-int2 eport))
			(v  (u16vector v1 v2)))
		   (values v (delay v))))
	       ((and (= count 2) (= type TIFF:type-ascii)) ; 1-char string
		 (let ((v 
			 (string (ascii->char (endian-port-read-int1 eport)))))
		   (endian-port-read-int1 eport) ; don't read '\0'
		   (endian-port-read-int1 eport) ; skip two more zeros:
		   (endian-port-read-int1 eport) ; padding
		   (values v (delay v))))
	       ((and (= count 2) (or (= type TIFF:type-byte)
				     (= type TIFF:type-undefined)))
		 (let* ((v1 (endian-port-read-int1 eport))
			(v2 (endian-port-read-int1 eport))
			(v  (u8vector v1 v2)))
		   (endian-port-read-int1 eport) ; skip two more zeros:
		   (endian-port-read-int1 eport) ; padding
		   (values v (delay v))))
	       ((and (= count 3) (= type TIFF:type-ascii)) ; 2-char string
		 (let* ((v1 (endian-port-read-int1 eport))
			(v2 (endian-port-read-int1 eport))
			(v  (string (ascii->char v1) (ascii->char v2))))
		   (endian-port-read-int1 eport) ; skip two more zeros:
		   (endian-port-read-int1 eport) ; padding
		   (values v (delay v))))
	       ((and (= count 3) (or (= type TIFF:type-byte)
				     (= type TIFF:type-undefined)))
		 (let* ((v1 (endian-port-read-int1 eport))
			(v2 (endian-port-read-int1 eport))
			(v3 (endian-port-read-int1 eport))
			(v  (u8vector v1 v2 v3)))
		   (endian-port-read-int1 eport) ; skip padding
		   (values v (delay v))))
	       ((and (= count 4) (= type TIFF:type-ascii)) ; 3-char string
		 (let* ((v1 (endian-port-read-int1 eport))
			(v2 (endian-port-read-int1 eport))
			(v3 (endian-port-read-int1 eport))
			(v  (string (ascii->char v1) (ascii->char v2)
			      (ascii->char v3))))
		   (endian-port-read-int1 eport) ; skip padding
		   (values v (delay v))))
	       ((and (= count 4) (or (= type TIFF:type-byte)
				     (= type TIFF:type-undefined)))
		 (let* ((v1 (endian-port-read-int1 eport))
			(v2 (endian-port-read-int1 eport))
			(v3 (endian-port-read-int1 eport))
			(v4 (endian-port-read-int1 eport))
			(v  (u8vector v1 v2 v3 v4)))
		   (values v (delay v))))
	       (else
		 (let ((offset (endian-port-read-int4 eport)))
		   (if (= type TIFF:type-ascii) 
		     (values offset (delay (read-string offset)))
		     (values offset (delay (read-array offset))))))))
	   ; Now count is 1
	   ((or (= type TIFF:type-byte) (= type TIFF:type-undefined)
	        (= type TIFF:type-sbyte))
	     (let ((v1 (endian-port-read-int1 eport)))
	       (endian-port-read-int1 eport) ; skip the padding
	       (endian-port-read-int1 eport)
	       (endian-port-read-int1 eport)
	       (values v1 (delay 
			   (if (= type TIFF:type-sbyte) (u8->s8 v1) v1)))))
	   ((= type TIFF:type-ascii)	; 0-byte string: count=1 for terminator
	     (endian-port-read-int1 eport) ; skip the padding
	     (endian-port-read-int1 eport)
	     (endian-port-read-int1 eport)
	     (endian-port-read-int1 eport)
	     (values "" (delay "")))
	  ((or (= type TIFF:type-short) (= type TIFF:type-sshort))
	    (let ((v1 (endian-port-read-int2 eport)))
	      (endian-port-read-int1 eport) ; skip the padding
	      (endian-port-read-int1 eport)
	      (values v1 (delay 
			  (if (= type TIFF:type-sshort) (u16->s16 v1) v1)))))
	   ((= type TIFF:type-long) 
	     (let ((v1 (endian-port-read-int4 eport)))
	       (values v1 (delay v1))))
	   ((= type TIFF:type-slong) 
	     (let ((v1 (endian-port-read-int4 eport)))
	       (values v1 (delay (u32->s32 v1)))))
	   ((= type TIFF:type-float) 
	     (let ((v1 (endian-port-read-int4 eport)))
	       (values v1 (delay (u32->float v1)))))
	   ((= type TIFF:type-double) 
	     (let ((offset (endian-port-read-int4 eport)))
	       (values offset (delay (read-double offset)))))
	   ((or (= type TIFF:type-rational) (= type TIFF:type-srational))
	     (let ((offset (endian-port-read-int4 eport)))
	       (values offset 
		 (delay
		   (let* ((_  (endian-port-setpos eport offset))
			  (v1 (endian-port-read-int4 eport))
			  (v2 (endian-port-read-int4 eport)))
		     (if (= type TIFF:type-srational)
		       (/ (u32->s32 v1) (u32->s32 v2))
		       (/ v1 v2)))))))
	   (else (delay (error "unknown data type: " type))))))
      (make-tiff-dir-entry tag type count val-offset value)
    )))

; procedure: print-tiff-dir-entry TIFF-DIR-ENTRY TAGDICT OPORT -> UNSPECIFIED
;
; Print the contents of TIFF-DIR-ENTRY onto the output port OPORT
; using TAGDICT to convert tag identifiers to symbolic names
(define (print-tiff-dir-entry tiff-dir-entry tagdict oport)
  (define (dspl . args) (for-each (lambda (item) (display item oport)) args))
  (let* ((tag-num (tiff-dir-entry-tag tiff-dir-entry))
	 (tag-symbol (tagdict-get-by-num tagdict tag-num)))
    (dspl
      (or tag-symbol
	(string-append "private tag " (number->string tag-num))))
    (dspl  ", count " (tiff-dir-entry-count tiff-dir-entry)
           ", type ")
    (let ((type-str
	    (any?
	      (lambda (elem)
		(and (= (cadr elem) (tiff-dir-entry-type tiff-dir-entry))
		  (caddr elem)))
	      TIFF:types)))
      (if type-str
	(dspl type-str)
	(dspl "unknown (" (tiff-dir-entry-type tiff-dir-entry) ")")))
    (let ((val-offset (tiff-dir-entry-val-offset tiff-dir-entry)))
      (dspl ", value-offset " val-offset)
      (if (integer? val-offset)
	(dspl " (0x" (number->string val-offset 16) ")")))
    (dspl nl)))

     
;------------------------------------------------------------------------
;                      TIFF Image File Directory
; TIFF directory is a collection of TIFF directory entries. The entries
; are sorted in an ascending order by tag.
; Note, a TIFF file can contain more than one directory (chained together).
; We handle only the first one.
;
; We treat a TIFF image directory somewhat as an ordered, immutable,
; dictionary collection, see SRFI-44.

; http://srfi.schemers.org/srfi-44/srfi-44.html

(define-structure tiff-directory entries tagdict)

; ; procedure: collection-name collection => symbol ('%)
; (define (collection-name coll)
;   (and (tiff-directory? coll) 'tiff-directory))

; ; collection? value => value
; (define collection? tiff-directory?)

; ; procedure: tiff-directory? value => bool
; ; implied by the define-structure

; ; *-size collection => integer
(define (tiff-directory-size coll)
  (vector-length (tiff-directory-entries coll)))

; (define (mutable-collection? coll) #f)
; (define (dictionary? coll) #t)

(define (tiff-directory-empty? coll)
  (zero? (vector-length (tiff-directory-entries coll))))


; tiff-directory-fold-left tiff-directory fold-function seed-value
;             ... => seed-value ...
; The fold function receives a tiff-directory-entry as a value

(define (tiff-directory-fold-left coll fn . seeds)
  (let ((entries (tiff-directory-entries coll)))
    (let loop ((i 0) (seeds seeds))
      (if (>= i (vector-length entries))
	(apply values seeds)
	(let*-values
	  (((proceed? . seeds) (apply fn (vector-ref entries i) seeds)))
	  (loop (if proceed? (+ 1 i) (vector-length entries))
	    seeds))))))

;             procedure: collection-fold-keys-left collection fold-function
;             seed-value ... => seed-value ...
; *-keys->list dictionary => list

           
; read-tiff-file EPORT [PRIVATE-TAGDICT] => TIFF-DIRECTORY
(define (read-tiff-file eport . tag-dict-opt)
  (endian-port-setpos eport (TIFF:read-header eport))
  (let ((entries (make-vector (endian-port-read-int2 eport)))
	(tagdict (if (null? tag-dict-opt) 
		   tiff-standard-tagdict
		   (tagdict-add-all tiff-standard-tagdict
		     (car tag-dict-opt)))))
    (do ((i 0 (+ 1 i))) ((>= i (vector-length entries)))
      (vector-set! entries i (TIFF:read-dir-entry eport)))
    (if (not (zero? (endian-port-read-int4 eport)))
      (cerr "The TIFF file contains several images, only the first one "
	"will be considered" nl))
    (make-tiff-directory entries tagdict)))


; print-tiff-directory TIFF-DIRECTORY OPORT -> UNSPECIFIED
(define (print-tiff-directory tiff-directory oport)
  (let*
    ((entries (tiff-directory-entries tiff-directory))
     (nentries (vector-length entries))
     (tagdict (tiff-directory-tagdict tiff-directory)))
    (for-each (lambda (item) (display item oport))
      (list
	"There are " nentries " entries in the TIFF directory" nl
	"they are" nl))
    (do ((i 0 (+ 1 i))) ((>= i nentries))
      (print-tiff-dir-entry (vector-ref entries i) tagdict oport))))


; *-get dictionary key [absence-thunk] => value
; key can be either a symbol or an integer
; tiff-directory-get TIFF-DIRECTORY KEY [ABSENCE-THUNK] -> VALUE
; If the lookup fails, ABSENCE-THUNK, if given, is evaluated and its value
; is returned. If ABSENCE-THUNK is omitted, the return value on failure
; is #f.
(define (tiff-directory-get coll key . default-val)
  (let* 
    ((key
       (cond
	 ((symbol? key)
	   (tagdict-get-by-name (tiff-directory-tagdict coll) key))
	 ((integer? key) key)
	 (else (error "tiff-directory-get: bad type of key: " key))))
      (entry
             ; look up the entry in the directory of entries
             ; We could have used a binary search... On the other hand,
             ; the directory is usually not that big, so that binary
             ; search is kind of overkill
	(any? 
	  (lambda (curr-elem)
	    (and (= (tiff-dir-entry-tag curr-elem) key) curr-elem))
	  (tiff-directory-entries coll)))
      )
    (if entry
      (force (tiff-dir-entry-value entry))
      (and (not (null? default-val)) ((car default-val))))))


; tiff-directory-get-as-symbol TIFF-DIRECTORY KEY [ABSENCE-THUNK] -> VALUE
; KEY must be a symbol
; If it is possible, the VALUE is returned as a symbol, as translated
; by the tagdict.
(define (tiff-directory-get-as-symbol coll key . default-val)
  (let ((val (tiff-directory-get coll key)))
    (if val
      (if (integer? val)
	(let ((val-symbol
		(tagdict-tagval-get-by-num 
		  (tiff-directory-tagdict coll) key val)))
	  (or val-symbol val))
	val)			; val is not an integer: don't translate
      (and (not (null? default-val)) ((car default-val)))
      )))
    



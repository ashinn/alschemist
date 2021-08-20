
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Images - 3D arrays of width*height*color-channel

(define-record-type Image
  (%make-image array color-space)
  image?
  (array image-array)
  (color-space image-color-space))

(define (image-width image)
  (let ((domain (array-domain (image-array image))))
    (- (interval-upper-bound domain 1) (interval-lower-bound domain 1))))

(define (image-height image)
  (let ((domain (array-domain (image-array image))))
    (- (interval-upper-bound domain 0) (interval-lower-bound domain 0))))

(define (make-image width height . o)
  (let* ((storage-class (if (pair? o) (car o) u8-storage-class))
         (array (make-specialized-array
                 (make-interval (vector width height 3))
                 storage-class)))
    ;; http://www.w3.org/Graphics/Color/sRGB
    (%make-image array 'sRGB)))

(define (array->image array . o)
  (assert (and (specialized-array? array)
               (= 3 (array-dimension array))
               (zero? (interval-lower-bound (array-domain array) 2))
               (<= 3 (interval-upper-bound (array-domain array) 2) 4)))
  (%make-image array 'sRGB))

(define (image-channels image)
  (let ((channels
         (array-tile (image-array image)
                     (vector (image-width image) (image-height image) 1)))
        (domain
         (make-interval (vector (image-width image) (image-height image)))))
    (values (specialized-array-reshape (array-ref channels 0 0 0) domain)
            (specialized-array-reshape (array-ref channels 0 0 1) domain)
            (specialized-array-reshape (array-ref channels 0 0 2) domain))))

(define (image-alpha image)
  (let ((array (image-array image))
        (width (image-width image))
        (height (image-height image)))
    (and (= 4 (interval-upper-bound (array-domain array) 2))
        (let ((channels (array-tile array (vector width height 1))))
          (specialized-array-reshape (array-ref channels 0 0 3)
                                     (make-interval (vector width height)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pixmaps - 2D arrays of single-value pixels

(define-record-type Pixel-Format
  (%make-pixel-format storage-class bits masks shifts palette
                      rgb-encoder rgba-encoder rgb-decoder rgba-decoder)
  pixel-format?
  (storage-class pixel-format-storage-class)
  (bits pixel-format-bits)
  (masks pixel-format-masks)
  (shifts pixel-format-shifts)
  (palette pixel-format-palette)
  (rgb-encoder pixel-format-rgb-encoder)
  (rgba-encoder pixel-format-rgba-encoder)
  (rgb-decoder pixel-format-rgb-decoder)
  (rgba-decoder pixel-format-rgba-decoder))

(define (make-rgb-decoder masks shifts)
  (let ((mask0 (vector-ref masks 0))
        (shift0 (- (vector-ref shifts 0)))
        (mask1 (vector-ref masks 1))
        (shift1 (- (vector-ref shifts 1)))
        (mask2 (vector-ref masks 2))
        (shift2 (- (vector-ref shifts 2))))
    (lambda (pixel)
      (values (bitwise-and mask0 (arithmetic-shift pixel shift0))
              (bitwise-and mask1 (arithmetic-shift pixel shift1))
              (bitwise-and mask2 (arithmetic-shift pixel shift2))))))

(define (make-rgba-decoder masks shifts)
  (let ((mask0 (vector-ref masks 0))
        (shift0 (- (vector-ref shifts 0)))
        (mask1 (vector-ref masks 1))
        (shift1 (- (vector-ref shifts 1)))
        (mask2 (vector-ref masks 2))
        (shift2 (- (vector-ref shifts 2)))
        (mask3 (vector-ref masks 3))
        (shift3 (- (vector-ref shifts 3))))
    (lambda (pixel)
      (values (bitwise-and mask0 (arithmetic-shift pixel shift0))
              (bitwise-and mask1 (arithmetic-shift pixel shift1))
              (bitwise-and mask2 (arithmetic-shift pixel shift2))
              (bitwise-and mask3 (arithmetic-shift pixel shift3))))))

(define (make-rgb-encoder masks shifts)
  (let ((shift0 (vector-ref shifts 0))
        (shift1 (vector-ref shifts 1))
        (shift2 (vector-ref shifts 2)))
    (lambda (red green blue)
      (bitwise-ior (arithmetic-shift red shift0)
                   (arithmetic-shift green shift1)
                   (arithmetic-shift blue shift2)))))

(define (make-rgba-encoder masks shifts)
  (let ((shift0 (vector-ref shifts 0))
        (shift1 (vector-ref shifts 1))
        (shift2 (vector-ref shifts 2))
        (shift3 (vector-ref shifts 3)))
    (lambda (red green blue alpha)
      (bitwise-ior (arithmetic-shift red shift0)
                   (arithmetic-shift green shift1)
                   (arithmetic-shift blue shift2)
                   (arithmetic-shift alpha shift3)))))

(define (make-rgba-pixel-format storage-class bits . o)
  (let ((masks (if (pair? o)
                   (car o)
                   (vector-map (lambda (x) (- (expt 2 x) 1))
                               (make-vector 4 (quotient bits 4)))))
        (shifts (if (and (pair? o) (pair? (cdr o)))
                    (cadr o)
                    (list->vector
                     (fold (lambda (i acc)
                             (cons (- bits (* i (quotient bits 4))) acc))
                           '()
                           (iota 4 1))))))
    (%make-pixel-format storage-class bits masks shifts #f
                        (make-rgb-encoder masks shifts)
                        (make-rgba-encoder masks shifts)
                        (make-rgb-decoder masks shifts)
                        (make-rgba-decoder masks shifts))))

(define rgba8888-format
  (make-rgba-pixel-format u32-storage-class 32))

(define (make-rgb-decoder/palette palette)
  (lambda (pixel)
    ((pixel-format-rgb-decoder rgba8888-format)
     (u32vector-ref palette pixel))))

(define (make-rgba-decoder/palette palette)
  (lambda (pixel)
    ((pixel-format-rgba-decoder rgba8888-format)
     (u32vector-ref palette pixel))))

(define (make-rgb-encoder/palette palette decoder)
  ;; TODO: ultra-slow - implement a nearest neighbor lookup in a color cube
  (lambda (r g b)
    (let lp ((i (- (u32vector-length palette) 1))
             (nearest #f)
             (nearest-distance 1e100))
      (if (negative? i)
          nearest
          (let-values (((r2 g2 b2) (decoder (u32vector-ref palette i))))
            (let ((dist (+ (square (- r r2))
                           (square (- g g2))
                           (square (- b b2)))))
              (if (< dist nearest-distance)
                  (lp (- i 1) i dist)
                  (lp (- i 1) nearest nearest-distance))))))))

(define (make-rgba-encoder/palette palette decoder)
  (let ((encode (make-rgb-encoder/palette palette decoder)))
    (lambda (r g b a) (encode r g b))))

(define (make-palette-pixel-format storage-class bits palette)
  (let ((decoder (make-rgb-decoder/palette palette)))
    (%make-pixel-format storage-class bits #f #f palette
                        (make-rgb-encoder/palette palette decoder)
                        (make-rgba-encoder/palette palette decoder)
                        decoder
                        (make-rgba-decoder/palette palette))))

(define-record-type Pixmap
  (%make-pixmap array pixel-format)
  pixmap?
  (array pixmap-array)
  (pixel-format pixmap-pixel-format))

(define (make-pixmap width height . o)
  (let* ((pixel-format (if (pair? o) (car o) rgba8888-format))
         (array (make-specialized-array
                 (make-interval (vector width height))
                 (pixel-format-storage-class pixel-format))))
    (%make-pixmap array pixel-format)))

(define (pixmap-width pixmap)
  (let ((domain (array-domain (pixmap-array pixmap))))
    (- (interval-upper-bound domain 1) (interval-lower-bound domain 1))))

(define (pixmap-height pixmap)
  (let ((domain (array-domain (pixmap-array pixmap))))
    (- (interval-upper-bound domain 0) (interval-lower-bound domain 0))))

(define (array->pixmap array . o)
  (assert (and (specialized-array? array) (= 2 (array-dimension array))))
  (let ((pixel-format (if (pair? o) (car o) rgba8888-format)))
    (%make-pixmap array pixel-format)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conversions

;; Reshaping allows us to flatten the R, G, B components as adjacent
;; values in the same dimension, but for a pixmap we want to join them
;; into a single value.

(define (image->pixmap image . o)
  (let* ((pixel-format (if (pair? o) (car o) rgba8888-format))
         (src (image-array image))
         (width (image-width image))
         (height (image-height image))
         (w0 (interval-lower-bound (array-domain src) 1))
         (h0 (interval-lower-bound (array-domain src) 0))
         (dst (make-specialized-array
               (make-interval (vector height width))
               (pixel-format-storage-class pixel-format)))
         (getter (array-getter src))
         (rgb->pixel (pixel-format-rgb-encoder pixel-format))
         (setter (array-setter dst)))
    (do ((i 0 (+ i 1)))
        ((= i height)
         (%make-pixmap dst pixel-format))
      (do ((j 0 (+ j 1)))
          ((= j width))
        (let ((r (getter (- i h0) (- j w0) 0))
              (g (getter (- i h0) (- j w0) 1))
              (b (getter (- i h0) (- j w0) 2)))
          (setter (rgb->pixel r g b) i j))))))

(define (pixmap->image pixmap . o)
  (let* ((storage-class (if (pair? o) (car o) u32-storage-class))
         (src (pixmap-array pixmap))
         (w0 (interval-lower-bound (array-domain src) 1))
         (h0 (interval-lower-bound (array-domain src) 0))
         (width (pixmap-width pixmap))
         (height (pixmap-height pixmap))
         (dst (make-specialized-array
               (make-interval (vector height width 3))
               storage-class))
         (getter (array-getter src))
         (pixel->rgb (pixel-format-rgb-decoder
                      (pixmap-pixel-format pixmap)))
         (setter (array-setter dst)))
    (do ((i 0 (+ i 1)))
        ((= i height)
         (%make-image dst 'sRGB))
      (do ((j 0 (+ j 1)))
          ((= j width))
        (let-values (((r g b) (pixel->rgb (getter (- i h0) (- j w0)))))
          (setter r i j 0)
          (setter g i j 1)
          (setter b i j 2))))))





(define (int32->u8vector-le int)
  (let* ((u8v (make-u8vector 4 0)))
    (u8vector-set! u8v 0 (bitwise-and int 255))
    (u8vector-set! u8v 1 (bitwise-and (arithmetic-shift int -8) 255))
    (u8vector-set! u8v 2 (bitwise-and (arithmetic-shift int -16) 255))
    (u8vector-set! u8v 3 (bitwise-and (arithmetic-shift int -24) 255))
    u8v))

(define (int16->u8vector-le int)
  (let* ((u8v (make-u8vector 2 0)))
    (u8vector-set! u8v 0 (bitwise-and int 255))
    (u8vector-set! u8v 1 (bitwise-and (arithmetic-shift int -8) 255))
    u8v))

(define (sigma lower upper fn)
  (do ((i lower (+ i 1))
       (a (fn lower) (+ a (fn i))))
      ((>= i upper) a)
    #f))


(define (sample->int16 x)
  (fxmin
   32767
   (fxmax
    -32768
    (flonum->fixnum (fl* x 32768.0)))))
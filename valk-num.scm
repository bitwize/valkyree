(define (u16->u8vector-le v)
  (let* ((l (u16vector-length v))
	 (l2 (* l 2))
	 (nv (make-u8vector l2 0)))
    (let loop ((i 0))
      (cond
       ((>= i l)
	nv)
       (else (let* ((s (u16vector-ref v i))
		    (v1 (bitwise-and s 255))
		    (v2 (arithmetic-shift s -8))
		    (i2 (* i 2)))
	       
	       (u8vector-set! nv i2
			      v1)
	       (u8vector-set! nv (+ i2 1)
			      v2)
	       (loop (+ i 1))))))))

(define (u8->u16vector-le v)
  (let ((l (u8vector-length v)))
    (if (odd? l)
	(error
	 "u8vector must have even length to be converted to u16vector")
	(let* ((l2 (arithmetic-shift l -1))
	       (new-vector (make-u16vector l2 0)))
	  (let loop ((i 0))
	    (if (>= i l2)
		new-vector
		(let* ((j (arithmetic-shift i 1))
		       (s (u8vector-ref v j))
		       (t (u8vector-ref v (+ j 1))))
		  (u16vector-set! new-vector i
			       (bitwise-ior (arithmetic-shift t 8)
					    s))
		  (loop (+ i 1)))))))))



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
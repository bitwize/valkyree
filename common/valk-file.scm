; Raw format output functions. Write a raw format audio file to filename fl,
; which will be the first n samples of output of generator gen, where n is
; given by the variable samples. Little endian format.

; In mono (write-raw) or stereo (st-write-raw).

(define (s16->u8vector-le v)
  (let* ((l (s16vector-length v))
	 (l2 (* l 2))
	 (nv (make-u8vector l2 0)))
    (let loop ((i 0))
      (cond
       ((>= i l)
	nv)
       (else (let* ((s (signed->unsigned16 (s16vector-ref v i)))
		    (v1 (remainder s 256))
		    (v2 (quotient s 256))
		    (i2 (* i 2)))
	       
	       (u8vector-set! nv i2
			      v1)
	       (u8vector-set! nv (+ i2 1)
			      v2)
	       (loop (+ i 1))))))))

(define
  (write-raw fl gen t samplerate)
  (let* ((p (open-output-file fl))
	 (v (sound-render-s16vector gen t samplerate))
	 (nv (s16->u8vector-le v))
	 
	 (len (u8vector-length nv)))
    
    
    (write-subu8vector nv 0 len p)
    (close-output-port p)))

(define (st-write-raw fl gen t samplerate)
  (let* ((p (open-output-file fl))
	 (v (sound-render-s16vector-st gen t samplerate))
	 (nv (s16->u8vector-le v))
	 
	 (len (u8vector-length nv)))
    
    
    (write-subu8vector nv 0 len p)
    (close-output-port p)))


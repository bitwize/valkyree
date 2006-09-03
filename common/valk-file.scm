

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


; Raw format output functions. Write a raw format audio file to filename fl,
; which will be the first n samples of output of generator gen, where n is
; given by the variable samples. Little endian format.

; In mono (write-raw) or stereo (st-write-raw).


(define
  (write-raw fl gen t samplerate)
  (let* ((p (open-output-file fl))
	 (v (sound-render-u16vector gen t samplerate))
	 (nv (u16->u8vector-le v))
	 
	 (len (u8vector-length nv)))
    
    
    (write-subu8vector nv 0 len p)
    (close-output-port p)))

(define (st-write-raw fl gen t samplerate)
  (let* ((p (open-output-file fl))
	 (v (sound-render-u16vector-st gen t samplerate))
	 (nv (u16->u8vector-le v))
	 
	 (len (u8vector-length nv)))
    
    
    (write-subu8vector nv 0 len p)
    (close-output-port p)))

(define (write-wav fl gen t samplerate)
  (values))


(define (read-chunk port size)
  (let* ((vec (make-u8vector size))
	 (r (read-subu8vector vec 0 size port)))
    (if (= r size)
	vec
	(subu8vector vec 0 r))))



(define (file->u8vector fl)
  (let ((p (open-input-file fl)))
    (let loop ((l '()))
      (let ((chunk (read-chunk p 4096)))
	(if (zero? (u8vector-length chunk))
	    (apply u8vector-append (reverse l))
	    (loop (cons chunk l)))))))

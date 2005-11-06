; Raw format output functions. Write a raw format audio file to filename fl,
; which will be the first n samples of output of generator gen, where n is
; given by the variable samples. Little endian format.

; In mono (write-raw) or stereo (st-write-raw).

(define
  (write-raw fl gen samples)
  (let* ((p (open-output-file fl))
	 (v (sound-render-s16vector gen samples)))
    
    (let loop ((i 0))	
      (cond
       ((>= i samples) #t)
       (#t (let* ((s  (signed->unsigned16 (s16vector-ref v i))))
	     (write-byte (remainder s 256) p)
	     (write-byte (quotient s 256) p)

	     (loop (+ i 1))))))))

(define (st-write-raw fl gen samples)
  (let* ((p (open-output-file fl))
	 (v (sound-render-s16vector-st gen samples))
	 (l (s16vector-length v)))
    
    (let loop ((i 0))
      (cond
       ((>= i samples) #t)
       (#t (let* ((s  (signed->unsigned16 (s16vector-ref v i))))
	     (write-byte (remainder s 256) p)
	     (write-byte (quotient s 256) p)

	     (loop (+ i 1)))))
      )))

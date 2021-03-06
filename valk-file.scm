
; Raw format output functions. Write a raw format audio file to filename fl,
; which will be the first n samples of output of generator gen, where n is
; given by the variable samples. Little endian format.

; In mono (write-raw) or stereo (st-write-raw).


(define
  (write-raw fl gen t samplerate)
  (let* ((p (open-output-file fl))
	 (v (sig-get-fragment/s16vector gen 0.0 t samplerate))
	 (nv (s16vector->u8vector/le v))
	 
	 (len (u8vector-length nv)))    
    (write-subu8vector nv 0 len p)
    (close-output-port p)))

(define (stereo-write-raw fl gen t samplerate)
  (let*  ((p (open-output-file fl))
	 (v (stereo-sig-get-fragment/s16vector gen 0.0 t samplerate))
	 (nv (s16vector->u8vector/le v))
	 (len (u8vector-length nv)))
    (write-subu8vector nv 0 len p)
    (close-output-port p)))

(define (write-wav fl gen t samplerate)
  (let* ((p (open-output-file fl))
	 (v (sig-get-fragment/s16vector gen 0.0 t samplerate))
	 (nv (s16vector->u8vector/le v))
	 (wd (make-wave-descriptor 1 1 samplerate (* samplerate 4) 4 16 nv))
	 (wv (descriptor->wave-contents wd))
	 (len (u8vector-length wv)))
    (write-subu8vector wv 0 len p)
    (close-output-port p)))

(define (stereo-write-wav fl gen t samplerate)
  (let* ((p (open-output-file fl))
	 (v (stereo-sig-get-fragment/s16vector gen 0.0 t samplerate))
	 (nv (s16vector->u8vector/le v))
	 (wd (make-wave-descriptor 1 1 samplerate (* samplerate 2) 2 16 nv))
	 (wv (descriptor->wave-contents wd))
	 (len (u8vector-length wv)))
    (write-subu8vector wv 0 len p)
    (close-output-port p)))

(define (write-ascii-wavetable fl gen t samplerate)
  (let* ((p (open-output-file fl))
	 (v (sig-get-fragment gen 0.0 t samplerate))
	 (len (sample-vector-length v)))
    (do ((i 0 (+ i 1)))
	((>= i len) (close-output-port p))
      (write (sample-vector-ref v i) p)
      (newline p))))

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

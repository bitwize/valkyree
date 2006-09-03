; RIFF format construction and analysis routines. Used to read/write .WAV
; files.

(define-record-type <wave-descriptor>
  (make-wave-descriptor fmt channels sample-rate byte-rate block-align bits-per-sample data)
  wave-descriptor?
  (fmt wave-descriptor-fmt)
  (channels wave-descriptor-channels)
  (sample-rate wave-descriptor-sample-rate)
  (byte-rate wave-descriptor-byte-rate)
  (block-align wave-descriptor-block-align)
  (bits-per-sample wave-descriptor-bits-per-sample)
  (data wave-descriptor-data)
  )

(define-record-type <riff-chunk>
  (make-riff-chunk type data)
  riff-chunk?
  (type riff-chunk-type)
  (data riff-chunk-data))

(define (riff-chunk-section chunk beg end)
  (subu8vector (riff-chunk-data chunk)
	       beg end))

(define (check-chunk-type chunk type)
  (values))

(define (word-aligned-size int)
  (+ int
     (remainder int
		2)))

(define (riff-chunk-size chunk)
  (+ (word-aligned-size (u8vector-length (riff-chunk-data chunk)))
     8))


(define (u8vector->int32-le u8vec)
  (bitwise-ior
   (u8vector-ref u8vec 0)
   (arithmetic-shift (u8vector-ref u8vec 1)
		     8)
   (arithmetic-shift (u8vector-ref u8vec 2)
		     16)
   (arithmetic-shift (u8vector-ref u8vec 3)
		     24)))

(define (u8vector->int16-le u8vec)
  (bitwise-ior
   (u8vector-ref u8vec 0)
   (arithmetic-shift (u8vector-ref u8vec 1)
		     8)))


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

(define (u8vector->riff-type u8vec)
  (string
   (ascii->char (u8vector-ref u8vec 0))
   (ascii->char (u8vector-ref u8vec 1))
   (ascii->char (u8vector-ref u8vec 2))
   (ascii->char (u8vector-ref u8vec 3))))

(define (riff-type->u8vector str)
  (u8vector
   (char->ascii (string-ref str 0))
   (char->ascii (string-ref str 1))
   (char->ascii (string-ref str 2))
   (char->ascii (string-ref str 3))))

(define (build-chunk u8vec . rest)
  (let* ((offs (if (null? rest)
		   0
		   (car rest)))
	 (type (u8vector->riff-type (subu8vector u8vec offs (+ offs 4))))
	 (size (u8vector->int32-le (subu8vector u8vec (+ offs 4) (+ offs 8))))
	 (data (subu8vector u8vec (+ offs 8) (+  offs 8 size))))    
    (make-riff-chunk type data)))

(define (build-main-chunk u8vec)
  (let* (
	 (type (u8vector->riff-type (subu8vector u8vec 8 12)))
	 (size (- (u8vector->int32-le  (subu8vector u8vec 4 8)) 4))
	 (data (subu8vector u8vec 12 (+  12 size))))
    
    (make-riff-chunk type data)))

(define (destructure-fmt-chunk chunk)
  (let* ((fmt (u8vector->int16-le (riff-chunk-section chunk 0 2)))
	 (channels (u8vector->int16-le (riff-chunk-section chunk 2 4)))
	 (sample-rate (u8vector->int16-le (riff-chunk-section chunk 4 8)))
	 (byte-rate (u8vector->int16-le (riff-chunk-section chunk 8 12)))
	 (block-align (u8vector->int16-le (riff-chunk-section chunk 12 14)))
	 (bits-per-sample (u8vector->int16-le (riff-chunk-section chunk 14 16))))
    (values fmt channels sample-rate byte-rate block-align bits-per-sample)))

(define (build-wave-descriptor u8vec)
  (let* ((wc (build-main-chunk u8vec)))
    (check-chunk-type wc "WAVE")
    (let* ((fmtc (build-chunk (riff-chunk-data wc)))
	   (datc (build-chunk (riff-chunk-data wc)
			      (riff-chunk-size fmtc))))
      (check-chunk-type fmtc "fmt ")
      (check-chunk-type datc "data")
      (call-with-values (lambda ()
			  (destructure-fmt-chunk fmtc))
	(lambda (f ch sr br ba bs)
	  (make-wave-descriptor f ch sr br ba bs (riff-chunk-data datc)))))))

(define (dump-wave-info port wd)
  (for-each (lambda (x)
	      (display (car x)
		       port)
	      (display ": " port)
	      (display (cadr x)
		       port)
	      (newline port))
	    `(("format" ,(if (= (wave-descriptor-fmt wd)
				1)
			     "PCM"
			     "unsupported"))
	      ("channels" ,(wave-descriptor-channels wd))
	      ("sample rate" ,(wave-descriptor-sample-rate wd))
	      ("byte rate" ,(wave-descriptor-byte rate wd))
	      ("block align" ,(wave-descriptor-block-align wd))
	      ("bits per sample" ,(wave-descriptor-bits-per-sample wd))
	      ("data length" ,(u8vector-length (wave-descriptor-data wd))))))


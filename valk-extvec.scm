; This is Valkyree's library of "external vector" routines.

(define (sigval->s16 x)
   (inexact->exact
    (floor
     (max -32768.0
	  (min 32767.0 (* x 32768.0))))))

(define (sigval->u16 x)
  (+ (sigval->s16 x) 32768))

(define (s16->sigval x)
  (exact->inexact (/ x 32768.0)))

(define (u16->sigval x)
  (s16->sigval (- x 32768)))

(define (sample-vector->s16vector sv)
  (let* ((l (sample-vector-length sv))
	 (ev (make-s16vector l)))
    (let loop ((i 0))
      (cond
       ((>= i l) ev)
       (else
	(begin (s16vector-set! ev i (sigval->s16 (sample-vector-ref sv i)))
	       (loop (+ i 1))))))))

(define (s16vector->sample-vector ev samplerate)
  (let* ((l (s16vector-length ev))
	 (fv (make-f32vector l)))
    (let loop ((i 0))
      (cond
       ((>= i l) (really-make-sample-vector fv samplerate))
       (else 
	(begin (f32vector-set! fv i (s16->sigval (s16vector-ref ev i)))
	       (loop (+ i 1))))))))

(define (sample-vector->u16vector sv)
  (let* ((l (sample-vector-length sv))
	 (ev (make-u16vector l)))
    (let loop ((i 0))
      (cond
       ((>= i l) ev)
       (else
	(begin (u16vector-set! ev i (sigval->u16 (sample-vector-ref sv i)))
	       (loop (+ i 1))))))))

(define (u16vector->sample-vector ev samplerate)
  (let* ((l (u16vector-length ev))
	 (fv (make-f32vector l)))
    (let loop ((i 0))
      (cond
       ((>= i l) (really-make-sample-vector fv samplerate))
       (else 
	(begin (f32vector-set! fv i (u16->sigval (u16vector-ref ev i)))
	       (loop (+ i 1))))))))

(define (make-stereo-sig-get-fragment/extvec constructor converter setter)
  (lambda (sig start length freq)
    (let* ((vl (flonum->fixnum (truncate (* len freq 2))))
	   (v1 (constructor vl))
	   (freq2 (fixnum->flonum freq)))
      
      (let loop
	  ((i 0))
	(cond
	 ((>= i vl) v1)
	 (else (begin (receive (l r)
				(sig (+ start (fl/
					       (fixnum->flonum i)
					       freq2)))
			       (setter v1 i (converter l))
			       (setter v1 (+ i 1) (converter r)))
		      (loop (+ i 2)))))))))

(define (make-sig-get-fragment/extvec constructor converter setter)
  (lambda (sig start length freq)
    (let* ((vl (flonum->fixnum (truncate (* len freq))))
	   (v1 (constructor vl))
	   (freq2 (fixnum->flonum freq)))
      
      (let loop
	  ((i 0))
	(cond
	 ((>= i vl) v1)
	 (else (begin 
		 (setter v1 i (converter 
			       (sig
				(+ start (fl/
					  (fixnum->flonum i)
					  freq2)))))
		 (loop (+ i 2)))))))))
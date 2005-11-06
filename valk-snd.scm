; Some important constants and period/frequency conversions.

(define *pi* (* (atan 1) 4))
(define *2pi* (* (atan 1) 8))
(define (freq->period f) (* f *2pi*))
(define (period->freq f) (/ f *2pi*))

; The basic Valkyree generator builder macro. You specify a function to output
; a waveform given a time parameter, i, and some other parameters; and it
; defines two functions: your original function and a generator constructor
; that takes the other parameters and returns a generator that takes only i.

(define-syntax define-valk-gen
  (syntax-rules ()
    ((_ name2 (name i p1 ...) body)
     (begin (define (name i p1 ...)
	      body)
	    (define (name2 p1 ...)
	      (lambda (i)
		(name i p1 ...)))))))

; Create a sine wave lookup table with samps samples

(define (make-sine-table samps)
  (let* ((subdiv (/ *2pi* samps))
	 (v (make-vector samps 0)))
    
    (let loop ((i 0)) (if (>= i samps) v
			  (begin
			    (vector-set! v i
					 (inexact->exact
					  (floor
					   (* 
					    (sin (* i subdiv))
					    32767))))
			    (loop (+ i 1)))))))


; Important frequencies in Valkyree: the resolution of the table, the sampling
; frequency, and the ratio between the two.

(define valkyree-freq
  (let* ((t 11025)
	 (s 44100))
    
    (lambda (sel)
      (cond
       ((eq? sel 'table) t)
       ((eq? sel 'samples) s)
       ((eq? sel 'ratio) (/ t s))
       (#t #f)))))

; A default sine wave table.

(define *sine-table*
  (make-sine-table (valkyree-freq 'table)))


(define (lookup-sin i)
  (vector-ref *sine-table* (remainder i (valkyree-freq 'table))))


; Sine wave generator.

(define-valk-gen sine-gen (sine-wave i freq)
  (lookup-sin (inexact->exact
	       (floor (* i freq (valkyree-freq 'ratio))))))

; Square wave generator.

(define-valk-gen square-gen (square-wave i freq)
  (let*  ((s (inexact->exact (round (/ (valkyree-freq 'samples) freq))))
	 (hs (/ s 2)))
    
    (if (< (remainder i s) hs) -32767 32767)))

; Amplitude changer. Multiply amplitude of output of generator func by factor.

(define-valk-gen change-ampl-gen (change-ampl i func factor)
  (let* ((r (inexact->exact (floor (* (func i) factor)))))
    (if (> (abs r) 32767) (* 32767 (if (< r 0) -1 1))
	r)))

; Sample offset generator. Shifts the output of func forward in time by samps
; samples.

(define-valk-gen sample-offset-gen (sample-offset i func samps)
  (let* ((i2 (- i samps)))
    (if (< i2 0)
	0
	(func i2))))


(define (signed->unsigned16 n)
  (if (negative? n)
      (+ 65536 n)
      n))

(define (sound-render-s16vector gen samples)
  (let* ((v (make-s16vector samples)))
    (let loop ((i 0))
      (cond ((>= i samples)
	     v)
	    (#t (begin (s16vector-set! v i (max -32768 (min 32767 (gen i))))
		       (loop (+ i 1))))))))

(define (sound-render-s16vector-st gen samples)
  (let* ((s2 (* samples 2))
	 
	 (v (make-s16vector s2)))
    (let loop ((i 0))
      (cond ((>= i s2)
	     v)
	    (#t (let* ((r (gen i)))
		  (s16vector-set! v i (max -32768 (min 32767 (car r))))
		  (s16vector-set! v (+ i 1) (max -32768 (min 32767 (cadr r))))
		  (loop (+ i 2))))))))

; Mix generator. Mixes the output of two generators together.

(define-valk-gen mix-gen (mix i funcs)
  (apply +
	 (map (lambda (f) (f i)) funcs)))

; Converts mono output to stereo output.

(define-valk-gen mono->stereo-gen (mono->stereo i f)
  (let* ((p (change-ampl i f 0.5)))
    (list p p)))

; Stereo mix generator: same as above but with 2 channels.

(define-valk-gen stereo-mix-gen (stereo-mix i funcs)
  (letrec ((st2+ (lambda (x y)
		    (list (+ (car x)
			      (car y))
			  (+ (cadr x)
			     (cadr y)))))
	   (st+ (lambda args (if (null? args)
				 '(0 0)
				 (st2+ (car args)
				        (apply st+ (cdr args)))))))
      (apply st+
	 (map (lambda (f) (f i)) funcs))))

; Pan generator. Converts a mono signal to stereo; lets you move it left or
; right.

(define-valk-gen pan-gen (pan i f p)
  (cond
   ((zero? p) (mono->stereo i f))
   ((<= p -1)
    (list (f i)
	  0
	  ))
   ((>= p 1)
    (list 0 (f i)))
   (#t

      (list (change-ampl i f (- 0.5 (/ p 2))
			      )
	    (change-ampl i f (+ 0.5 (/ p 2))
			      )))))


; Some important constants and frequency conversions.

(define *pi* (* (atan 1) 4))
(define *2pi* (* (atan 1) 8))
(define (ang-freq f) (* f *2pi*))


(define (constantly x)
  (lambda t
    x))

(define silence (constantly 0))


; Sine wave generator.
; Produces a waveform according to the following function:
; s(t) = A cos ωt + p where ω = 2πf

(define (sine-wave freq ampl . phase)
  (let ((phase (if (null? phase)
		   0
		   (car phase))))
    (lambda (t)
      (* ampl (cos (+ phase (ang-freq (* freq t))))))))


; Square wave generator.
;
; Produces a waveform according to the function:
;        | 1 if r is less than d
; s(t) = |
;        | 0 otherwise
;
; where r is where t is in the current period
;       d is the duty cycle
;
(define (square-wave freq ampl . duty)
  (let ((duty (if (null? duty)
		   0.5
		   (car duty))))
    (lambda (t)
      (let* ((t2 (* t freq))
	     (r (- t2 (floor t2))))
	(* ampl
	   (if (< r duty)
	       1.0
	       -1.0))))))

; Sawtooth wave generator.
; Produces a waveform that looks like this:
;     /|   /|   /|
;    / |  / |  / |
;   /  | /  | /  |
;  /   |/   |/   |

(define (saw-wave freq vel)
  (lambda (t)
    (let* ((t2 (* freq t)))
      (* 2 vel (- t2
		  (floor (+ 0.5 t2)))))))

; ADSR (attack, decay, sustain, release) envelope structure.
; Useful for making ADSR functions to control the volume output of notes.

(define-record-type <adsr-envelope>
  (make-adsr-envelope a d s r)
  adsr-envelope?
  (a adsr-attack)
  (d adsr-decay)
  (s adsr-sustain)
  (r adsr-release))

(define (ramp start end start-time length t)
  (if (> t start-time)
      (if (or (<= length 0)
	      (>= t (+ start-time length)))
	  end
	  (+ start
	     (*
	      (/ (- end start)
		 length)
	      (- t start-time))))
      start))


(define (adsr-envelope-fun e len)
  (let ((a (adsr-attack e))
	(d (adsr-decay e))
	(s (adsr-sustain e))
	(r (adsr-release e)))
    (lambda (t)
      (cond
       ((< t 0)
	0)
       ((< t a)
	(ramp 0.0 1.0 0 a t))
       ((< t (+ a d))
	(ramp 1.0 s a d t))
       ((< t len)
	s)
       (else
	(ramp s 0.0 len r t))))))



(define (modulate f1 f2)
  (lambda (t)
    (* (f1 t)
       (f2 t))))

; Amplitude changer. Multiply amplitude of output of generator func by factor.

(define (change-ampl f factor)
  (modulate f (constantly factor)))

; Sample offset generator. Shifts the output of func forward in time by t
; seconds.

(define (sample-offset f offset)
  (lambda (t)
    (if (< t offset)
	0.0
	(f (- t offset)))))


; The u16clamp function takes a signal value and clamps it to the range
; [-1.0,1.0], then returns as a result an unsigned representation of the
; signed, clamped value scaled to an exact integer between -32768 and 32767.
; "It's gonna be clamp this, clamp that, bada climp, bada clamp!"
;                  --Clamps, from Futurama's Robot Mafia

(define (u16clamp x)
  (bitwise-and
   (inexact->exact
    (floor
     (* (min 1.0
	     (max -1.0 x))
	32767)))
   #xffff))


(define (sound-render-u16vector gen t samplerate)
  (let* (
	 (samples (inexact->exact (floor (* t samplerate))))
	 (samplerate (exact->inexact samplerate)) ; thanks to Brad Lucier
	 (sampleinc (/ 1.0 samplerate)) 
	 (v (make-u16vector (* samples))))
    (let loop ((i 0)
	       (j 0.0))
      (cond ((>= i samples)
	     v)
	    (else (begin (u16vector-set! v i
					 (u16clamp
					  (gen j)))       
		       (loop (+ i 1) (+ j sampleinc))))))))


(define (sound-render-f32vector gen t samplerate)
  (let* (
	 (samples (inexact->exact (floor (* t samplerate))))
	 (samplerate (exact->inexact samplerate))
	 (sampleinc (/ 1.0 samplerate)) 
	 (v (make-f32vector (* samples))))
    (let loop ((i 0)
	       (j 0.0))
      (cond ((>= i samples)
	     v)
	    (else (begin (f32vector-set! v i
					(gen j))
			 
		       (loop (+ i 1) (+ j sampleinc))))))))


(define (sound-render-u16vector-st gen t samplerate)
  (let* (
	 (samples (inexact->exact (floor (* t samplerate))))
	 (samplerate (exact->inexact samplerate))
	 (sampleinc (/ 1.0 samplerate)) 
	 (s2 (* samples 2))
	 
	 (v (make-u16vector s2)))
    (let loop ((i 0)
	       (j 0.0))
      (cond ((>= i samples)
	     v)
	    (else (call-with-values (lambda ()
				    (gen j))
		  
		    (lambda (a b)
		      (u16vector-set! v (* i 2)
				     (u16clamp a))
		      (u16vector-set! v (+ (* i 2) 1)
				       (u16clamp b))))
		  (loop (+ i 1) (+ j sampleinc)))))))

(define (sound-render-f32vector-st gen t samplerate)
  (let* (
	 (samples (inexact->exact (floor (* t samplerate)))) 
	 (samplerate (exact->inexact samplerate))
	 (sampleinc (/ 1.0 samplerate)) 

	 (s2 (* samples 2))
	 
	 (v (make-f32vector s2)))
    (let loop ((i 0) (j 0.0))
      (cond ((>= i samples)
	     v)
	    (else (call-with-values (lambda ()
				    (gen j))
		  
		    (lambda (a b)
		      (f32vector-set! v (* i 2)
				      a)
		      (f32vector-set! v (+ (* i 2) 1)
				      b)))
		  (loop (+ i 1) (+ j sampleinc)))))))


; Mix generator. Mixes the output of two generators together.

(define (mix f1 f2)
  (lambda (t)
    (+ (f1 t)
       (f2 t))))

; Stereo mix generator: same as above but with 2 channels.

(define (stereo-mix f1 f2)
  (lambda (t)
    (call-with-values (lambda ()
			(f1 t))
      (lambda (a b)
	(call-with-values (lambda ()
			    (f2 t))
	  (lambda (c d)
	    (values (+ a c)
		    (+ b d))))))))

(define (stereo left right)
  (lambda (t)
    (values (left t)
	    (right t))))

; Converts mono output to stereo output.

(define (mono->stereo f)
  (let* ((f2 (change-ampl f 0.5)))
    (stereo f2 f2)))

(define (left-channel f)
  (lambda (t)
    (call-with-values (lambda ()
			(f t))
      (lambda (a b)
	a))))

(define (right-channel f)
  (lambda (t)
    (call-with-values (lambda ()
			(f t))
      (lambda (a b)
	b))))

(define (stereo-change-ampl f factor)
  (stereo
   (change-ampl (left-channel f)
		factor)
   (change-ampl (right-channel f)
		factor)))

; Pan generator. Converts a mono signal to stereo; lets you move it left or
; right.
; -1.0 is full left, +1.0 is full right.
(define (pan f factor)
  (let* ((fac2 (* factor 0.5)))
    (stereo
     (change-ampl f
		  (- 0.5 fac2))
     (change-ampl f (+ 0.5 fac2)))))


(define (unsigned->signed16 n) (if (> n 32767) (- n 65536) n))

;(define (unsigned->signed16 n)
;(- n (arithmetic-shift (bitwise-and n 32768)
;1)))



(define (sampled-sound-u16 vec rate)
  (let* ((l (u16vector-length vec)))
    (lambda (i)
      (let ((c (inexact->exact (floor (* i rate)))))
	(if (or
	     (< c 0)
	     (>= c l))
	    0
	    (/ (unsigned->signed16 (u16vector-ref vec c))
	       32767.0))))))


(define (sampled-sound vec rate)
  (let* ((l (f32vector-length vec)))
    
    
    (lambda (i)
      (let ((c (inexact->exact (floor (* i rate)))))
	(if (or (< c 0)
		(>= c l))
	    0
	    (f32vector-ref vec c))))))


(define (sampled-sound-looping vec rate)
  (let* ((l (f32vector-length vec)))  
    (lambda (i)
      (let ((c (inexact->exact (floor (* i rate)))))
	(if (< c 0)
	    0
	    (f32vector-ref vec (modulo c l)))))))

(define (make-wavetable func nsamps)
  (sound-render-f32vector func 1.0 nsamps))

(define (harmonics . x)
  (let loop
      ((i 1.0)
       (l x)
       (f silence))
    
    (cond
     ((null? l)
      f)
     (else
      (loop (+ i 1)
	    (cdr l)
	    (mix f (sine-wave i (car l))))))))

(define (wavetable-function func nsamps)
  (let* ((wt (make-wavetable func nsamps)))
    (lambda (freq vol)
      (change-ampl (sampled-sound-looping wt (* freq nsamps))
		   vol))))


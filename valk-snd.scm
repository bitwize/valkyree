; Some important constants and frequency conversions.

(define *pi* (* (atan 1) 4))
(define *2pi* (* (atan 1) 8))
(define (ang-freq f) (* f *2pi*))


(define (constantly x)
  (lambda t
    x))

(define silence (constantly 0))


; Sine wave oscillator.
; Produces a waveform according to the following function:
; s(t) = A cos ωt + p where ω = 2πf

(define (sine-oscillator freq ampl phase)

    (lambda (t)
      (* ampl (cos (+ phase (ang-freq (* freq t)))))))


; Square wave oscillator.
;
; Produces a waveform according to the function:
;        | 1 if r is less than d
; s(t) = |
;        | 0 otherwise
;
; where r is where t is in the current period
;       d is the duty cycle
;
(define (square-oscillator freq ampl duty)

    (lambda (t)
      (let* ((t2 (* t freq))
	     (r (- t2 (floor t2))))
	(* ampl
	   (if (< r duty)
	       1.0
	       -1.0)))))

; Sawtooth wave oscillator.
; Produces a waveform that looks like this:
;     /|   /|   /|
;    / |  / |  / |
;      | /  | /  | /
;      |/   |/   |/

(define (saw-oscillator freq ampl)
  (lambda (t)
    (let* ((t2 (* freq t)))
      (* 2 ampl
	 (- t2 (floor (+ 0.5 t2)))))))

(define (triangle-oscillator freq ampl)
  (lambda (t)
    (let ((t (* t freq)))
      (* ampl
	 (- 1 (* 4 (let ((r (+ t 0.25)))
		  (abs (- 0.5 (- r (truncate r)))))))))))


; ADSR (attack, decay, sustain, release) envelope structure.
; Useful for making ADSR functions to control the volume output of notes.

(define-record-type :adsr-envelope
  (make-adsr-envelope a d s r)
  adsr-envelope?
  (a adsr-attack)
  (d adsr-decay)
  (s adsr-sustain)
  (r adsr-release))

(define (ramp start end start-time length)
  (let* ((fac (/ (- end start) length))
	 (end-time (+ start-time length)))
    (lambda (t)
      (if (> t start-time)
	  (if (or (<= length 0)
		  (>= t end-time))
	      end
	      (+ start
		 (*
		  fac
		  (- t start-time))))
	  start))))


(define (adsr-envelope-gen e len)
  (let* ((a (adsr-attack e))
	 (d (adsr-decay e))
	 (s (adsr-sustain e))
	 (r (adsr-release e))
	 (ramp-a (ramp 0.0 1.0 0.0 a))
	 (ramp-d (ramp 1.0 s a d))
	 (ramp-r (ramp s 0.0 len r)))
    
    (lambda (t)
      (cond
       ((< t 0)
	0)
       ((< t a)
	(ramp-a t))
       ((< t (+ a d))
	(ramp-d t))
       ((< t len)
	s)
       (else
	(ramp-r t))))))

; Mix generator. Mixes the output of two generators together.

(define (sig+ f1 f2)
  (lambda (t)
    (+ (f1 t)
       (f2 t))))

(define (sig* f1 f2)
  (lambda (t)
    (* (f1 t)
       (f2 t))))

; Amplitude changer. Multiply amplitude of output of generator func by factor.

(define (change-ampl f factor)
  (sig* f (constantly factor)))

; Pitch modulator. Alters pitch of generator g1 by factor from generator g2
; (and does so in a way that doesn't munge the waveform).

(define (pitch-modulate f1 f2)
  (let ((p #f)
	(r #f))
    (lambda (t)
      (let ((t2 (if (not p)
		    t
		    (+ p (* (f2 t) (- t r))))))
	(set! p t2)
	(set! r t)
	(f1 p)))))

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

; Stereo mix generator: same as above but with 2 channels.

(define (stereo-sig+ f1 f2)
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

(define (harmonics fund ampl . x)
  (let loop
      ((i 1.0)
       (l x)
       (f silence))
    
    (cond
     ((null? l)
      f)
     ((not (car l))
      (loop (+ i 1)
	    f
	    (cdr l)))
     (else
      (loop (+ i 1)
	    (cdr l)
	    (sig+ f (sig* (sine-oscillator (* fund i) ampl 0.0) (car l))))))))

(define (wavetable-function func nsamps)
  (let* ((wt (make-wavetable func nsamps)))
    (lambda (freq vol)
      (change-ampl (sampled-sound-looping wt (* freq nsamps))
		   vol))))


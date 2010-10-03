
(define (nes-noise>> freq)
  (define noise-shift
    (let ((n 1))
      (lambda ()
	(let ((v (bitwise-and n 1))
	    (v2 (bitwise-and (arithmetic-shift n -1) 1)))
	  (set! n
		(bitwise-ior
		 (arithmetic-shift n -1)
		 (arithmetic-shift
		  (bitwise-xor v v2)
		  14)))
	  (zero? v)))))
  (let ((wavelength (/ 1 freq))
	(prev-t 0)
	(val 0.0))
    (lambda (t)
      (let loop ()
	(if (> (- t prev-t) wavelength)
	    (begin
	      (set! prev-t (+ prev-t wavelength))
	      (set! val (if (noise-shift) 1.0 0.0))
	      (loop))))
	    
	val)))

(define sampled-nes-noise
  (play-sample-vector-looping 
   (sig-get-fragment (nes-noise>> 1.0) 0.0 32768.0 1)
   0.0 0.0 32768.0))

(define sampled-square-8th-duty
  (play-sample-vector-looping
   (sig-get-fragment (bl-square-oscillator 1.0 10 0.125) 0.0 1.0 256)
   0.0 0.0 1.0))

 (define sampled-square-4th-duty
  (play-sample-vector-looping
   (sig-get-fragment (bl-square-oscillator 1.0 10 0.25) 0.0 1.0 256)
   0.0 0.0 1.0))

(define sampled-square-half-duty
  (play-sample-vector-looping
   (sig-get-fragment (bl-square-oscillator 1.0 10 0.5) 0.0 1.0 256)
   0.0 0.0 1.0))

(define sampled-square-3-4ths-duty
  (play-sample-vector-looping
   (sig-get-fragment (bl-square-oscillator 1.0 10 0.75) 0.0 1.0 256)
   0.0 0.0 1.0))

(define sampled-triangle
  (play-sample-vector-looping
   (sig-get-fragment (ideal-triangle-oscillator 1.0 1.0) 0.0 1.0 256)
   0.0 0.0 1.0))
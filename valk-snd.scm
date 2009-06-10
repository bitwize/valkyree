; Welcome to Valkyree.

; Valkyree is a sound and music synthesis system designed to work with units
; of sound from the very basic -- simple waveforms -- to the most complex of
; compositions. It is intended to favor correctness over speed, although some
; basic optimizations should be easy to perform.

; "The design of the central data structure of an app determines the quality
; of the app, in every way." --Dave Winer

; With this thought in mind we will here introduce the two basic data struc-
; tures of Valkyree: signals, and sample-vectors.

; A signal is simply a function taking ℝ into ℝ, where the input represents
; time and the output represents a magnitude. This lets us describe waveforms
; in the abstract as functions of time. The magnitude of a signal is assumed to
; be within the range -1.0 to 1.0. Signals may exceed this range; however they
; will be clipped before output when rendering the sound.

; Signals are also often called unit generators.

; Some important constants and frequency conversions.

(define pi (* (atan 1) 4))
(define twopi (* (atan 1) 8))
(define (ang-freq f) (* f twopi))

; You can perform basic math on signals, such as adding them or multiplying
; them. You can also multiply a signal by a scalar value.

(define (sig+ f1 f2)
  (lambda (t)
    (fl+ (f1 t)
       (f2 t))))

(define (sig* f1 f2)
  (lambda (t)
    (fl* (f1 t)
	 (f2 t))))

(define (sig-scale f1 c)
  (let ((c (exact->inexact c)))
    (lambda (t)
      (fl* (f1 t)
	   c))))

; You can also scale signals in the other direction: the time direction.

(define (time-scale f1 c)
  (lambda (t)
    (f1 (* c t))))

; You can shift signals in time.
(define (time-shift f offset)
  (lambda (t)
	(f (- t offset))))

; You can delay a signal. Signals which are time-delayed forward are
; left-padded with silence.
(define (time-delay f offset)
  (lambda (t)
    (if (< t offset)
	0.0
	(f (- t offset)))))

; You can switch between one signal input and another at a specified time.

(define (sig-switch f1 f2 switch-time)
  (lambda (t)
    (if (fl> t switch-time)
	(f2 t)
	(f1 t))))

; You can modulate the pitch of one signal by another signal. The
; pitch-modulate>> procedure does this in a way that doesn't munge the
; waveform.

; The >> at the end is a sigil which indicates that the signal this procedure
; returns stores state in between its invocations, meaning that we can't
; guarantee the same value if called more than once with the same time t.
; It is best to use such signals only in instances where you know that t
; will be monotonically increasing or decreasing.

(define (pitch-modulate>> f1 f2)
  (let ((p #f)
	(r #f))
    (lambda (t)
      (let ((t2 (if (not p)
		    t
		    (+ p (* (f2 t) (- t r))))))
	(set! p t2)
	(set! r t)
	(f1 p)))))


; Some trivial signals which are actually constant functions.

(define (constantly x)
  (lambda t
    x))

(define silence (constantly 0.0))

; Here are some basic signals: oscillators which produce _ideal_ sine, square,
; sawtooth and triangle waves. The square, sawtooth, and triangle oscillators
; are completely fine for LFO purposes; but are not appropriate for use in
; tone generation because they are ideal and so have infinite harmonics. When
; sampled they will lead to aliasing. Later we will introduce oscillators which
; are more appropriate for use in tone generation.

(define (sine-oscillator freq ampl phase)
  (let ((freq (exact->inexact freq))
	(ampl (exact->inexact ampl))
	(phase (exact->inexact phase)))
    (lambda (t)
      (fl* ampl (sin (fl+ phase (ang-freq (fl* freq t))))))))


(define (ideal-square-oscillator freq ampl duty)
  (let ((freq (exact->inexact freq))
	(ampl (exact->inexact ampl))
	(duty (exact->inexact duty)))
    (lambda (t)
      (let* ((t2 (fl* t freq))
	     (r (fl- t2 (floor t2))))
	(fl* ampl
	     (if (fl< r duty)
		 1.0
		 -1.0))))))

(define (ideal-saw-oscillator freq ampl)
  (let ((freq (exact->inexact freq))
	(ampl (exact->inexact ampl)))
    (lambda (t)
      (let* ((t2 (fl* freq t)))
	(fl* 2. ampl
	     (fl- t2 (floor (fl+ 0.5 t2))))))))

(define (ideal-triangle-oscillator freq ampl)
  (let ((freq (exact->inexact freq))
	(ampl (exact->inexact ampl)))
    (lambda (t)
      (let ((t (fl* t freq)))
	(fl* ampl
	     (fl- 1. (fl* 4 (let ((r (fl+ t 0.25)))
			      (abs (fl- 0.5 (fl- r (truncate r))))))))))))

(define (harmonic-series base-freq . ampls)
  (let* ((amplvector (list->vector ampls))
	 (l (vector-length amplvector))
	 (sinevector
	  (do ((i 0 (+ i 1))
	       (vec (make-vector l)))
	      ((>= i l) vec)
	    (vector-set! vec i (sine-oscillator (* (fixnum->flonum (+ i 1))
						     base-freq)
						(vector-ref amplvector i)
						0.0))))
	 (sum-func
	  (do ((i 1 (+ i 1))
	       (f (vector-ref sinevector 0) (sig+ f (vector-ref sinevector i))))
	      ((>= i l) f)
	    #f)))
    sum-func))

(define (bl-saw-oscillator freq nharms)
  (sig-scale
   (do ((i 0 (+ i 1))
       (l '() (cons (fl/ 1.0 (exact->inexact (+ i 1))) l)))
      ((>= i nharms) (apply harmonic-series (cons freq (reverse l))))
    #f)
   (- (/ 2 pi))))

(define (bl-square-oscillator freq nharms duty)
  (let* ((sw1 (bl-saw-oscillator freq nharms))
	 (sw2 (time-shift (sig-scale sw1 -1.0) (fl* (fl/ 1.0 freq) (- duty)))))
    (sig+ sw1 sw2)))

; White noise generator. The noise is generated with a pseudo random
; number generator. Note the sigil; again, this procedure stores state
; implicitly in the RNG.

(define (white-noise>> ampl)
  (lambda (t)
    (* (random-real) ampl)))

; A special signal used for crafting ADSR envelopes and the like.

(define (ramp source target time)
  (if (<= time 0.0)
      (constantly target)
      (let ((fac (/ (- target source) time)))
	(lambda (t)
	  (+ source (* t fac))))))
  
; Stereo signals are functions from ℝ to ℝ × ℝ. In Scheme they yield two values
; according to the Scheme conventions for multiple-value return. We could
; construct an n-channel signal in this manner by changing the function's co-
; domain to ℝ^n, but for now Valkyree only provides easy handling for mono or
; 2-channel stereo.

; Here we provide facilities for constructing stereo signals from one or two
; mono signals, or destructuring a stereo signal into its two constituent
; channels.

(define (stereo left right)
  (lambda (t)
    (values (left t)
	    (right t))))

(define (mono->stereo f)
  (let* ((f2 (sig-scale f 0.5)))
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

; You can add two stereo signals, or multiply a signal by a scalar, as with
; mono signals.

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

(define (stereo-sig-scale f factor)
  (lambda (t)
    (receive (l r) (f t)
	     (values (* l factor) (* r factor)))))

; You can also create a stereo signal by panning a mono signal to the left or
; right.

(define (pan f factor)
  (let* ((fac2 (* factor 0.5)))
    (stereo
     (sig-scale f
		  (- 0.5 fac2))
     (sig-scale f (+ 0.5 fac2)))))

; A sample-vector is a vector of samples from a signal over some
; arbitrary interval, accompanied by a sampling frequency (in a whole
; number of hertz).  These samples represent some portion of a signal
; in the time domain. They are a more concrete representation of a
; digital signal; i.e., they are closer to your computer soundcard's
; representation of a signal as a sequence of sample values. They are
; thus also amenable to DSP techniques such as discrete
; frequency-domain transforms.

; Samples in a sample-vector are 32-bit floating point, signed,
; zero-centered, and unit-normalized. That means they range from -1.0
; to 1.0 with zero in the middle (again, they can exceed this range
; but if they do they'll be clipped before output). They are stored in
; an underlying SRFI 4 f32vector.

(define-record-type :sample-vector
  (make-sample-vector vec freq)
  sample-vector?
  (vec sample-vector-underlying-f32vector)
  (freq sample-vector-sampling-frequency))

(define (sample-vector-ref svec n)
  (f32vector-ref (sample-vector-underlying-f32vector svec) n))

(define (sample-vector-length svec)
  (f32vector-length (sample-vector-underlying-f32vector svec)))

; A "fragment" of a signal may be retrieved as a sample-vector; this
; is a vector of all the values of a signal in a given interval,
; sampled with a particular frequency.

(define (sig-get-fragment f start len freq)
  (let* ((vl (flonum->fixnum (truncate (* len freq))))
	 (v (make-f32vector vl))
	 (freq2 (fixnum->flonum freq)))

    (let loop
	((i 0))
      (cond
       ((>= i vl) (make-sample-vector v freq))
       (else (begin (f32vector-set! v i (f (+ start (fl/
						     (fixnum->flonum i)
						     freq2))))
		    (loop (+ i 1))))))))

(define (stereo-sig-get-fragment f start len freq)
  (let* ((vl (flonum->fixnum (truncate (* len freq))))
	 (v1 (make-f32vector vl))
	 (v2 (make-f32vector vl))
	 (freq2 (fixnum->flonum freq)))

    (let loop
	((i 0))
      (cond
       ((>= i vl) (values (make-sample-vector v1 freq)
			  (make-sample-vector v2 freq)))
       (else (begin (receive (l r)
			     (f (+ start (fl/
					  (fixnum->flonum i)
					  freq2)))
			     (f32vector-set! v1 i l)
			     (f32vector-set! v2 i r))
		    (loop (+ i 1))))))))

; You can play back some of the samples captured in a sample-vector as a signal.

(define (play-sample-vector svec start end)
  (let*
      ((freq (sample-vector-sampling-frequency svec))
       (ffreq (fixnum->flonum freq))
       (sstart (flonum->fixnum (truncate (fl* start ffreq))))
       (send (flonum->fixnum (truncate (fl* end ffreq)))))
    (lambda (t)
      (let ((st (fx+ (flonum->fixnum (truncate (fl* t ffreq))) sstart)))
	(if (and (> st 0) (< st send))
	    (sample-vector-ref svec st)
	    0.0)))))

; You can also play back a sample with a loop; the signal will repeatedly
; play back everything from the loop point to the end point, infinitely.

(define (play-sample-vector-looping svec start loop end)
  (let*
      ((freq (sample-vector-sampling-frequency svec))
       (ffreq (fixnum->flonum freq))
       (sstart (flonum->fixnum (truncate (fl* start ffreq))))
       (sloop (flonum->fixnum (truncate (fl* loop ffreq))))
       (send (flonum->fixnum (truncate (fl* end ffreq))))
       (sloop-length (fx- send sloop))
       (srun-length (fx- send sstart)))
    (lambda (t)
      (let ((st (fx+ (flonum->fixnum (truncate (fl* t ffreq))) sstart)))
	(if (> st 0)
	    (if (< st send)
		(sample-vector-ref svec st)
		(sample-vector-ref svec
				   (+ sloop (remainder (- st send) sloop-length))))
	    0.0)))))
  
; Stereo versions of the above.

(define (stereo-play-sample-vector svec1 svec2 start end)
  (stereo
   (play-sample-vector svec1 start end)
   (play-sample-vector svec2 start end)))

(define (stereo-play-sample-vector-looping svec1 svec2 start loop end)
  (stereo
   (play-sample-vector-looping svec1 start loop end)
   (play-sample-vector-looping svec2 start loop end)))

; A spectrum-vector is like a sample-vector, but in the frequency
; domain.  The sampling frequency represents the Nyquist frequency of
; the resulting spectrum (twice the max frequency in the spectrum).

(define-record-type :spectrum-vector
  (make-spectrum-vector vec freq)
  spectrum-vector?
  (vec spectrum-vector-underlying-f32vector)
  (freq spectrum-vector-sampling-frequency))

(define (spectrum-vector-ref svec n)
  (f32vector-ref (spectrum-vector-underlying-f32vector svec) n))

(define (spectrum-vector-length svec)
  (f32vector-length (spectrum-vector-underlying-f32vector svec)))

; Procedures to convert between the time and frequency domains using
; the discrete cosine transform types II and III. (Sometimes called
; DCT and inverse DCT.) These are strictly real-valued variations of
; the more general discrete Fourier transform.

; These implementations are both naïve and literal-minded. They are
; bound to be replaced with something more optimized in the future.

; The public Valkyree APIs for handling time and frequency domain
; conversions are in the samples->spectrum/* functions below.

(define (discrete-cosine-transform data-f32vector)
  (let* ((len (f32vector-length data-f32vector))
	 (dct-f32vector (make-f32vector len)))
    (let loop ((k 0))
      (cond
       ((>= k len) dct-f32vector)
       (else
	(f32vector-set! dct-f32vector k
			(*
			 (sqrt (/ (if (zero? k) 1.0 2.0) len))
			 (sigma 0 (- len 1)
				(lambda (n)
				  (*
				   (f32vector-ref data-f32vector n)
				   (cos (/ (* pi (+ n 0.5) k) len)))))))
	(loop (+ k 1)))))))


(define (inverse-discrete-cosine-transform dct-f32vector)
  (let* ((len (f32vector-length dct-f32vector))
	 (data-f32vector (make-f32vector len)))
    (let loop ((k 0))
      (cond
       ((>= k len) data-f32vector)
       (else
	(f32vector-set! 
	 data-f32vector k
	 (* (sqrt (/ 2.0 len))
	    (sigma 0 (- len 1)
		   (lambda (n)
		     (*
		      (f32vector-ref dct-f32vector n)
		      (cos (/ (* pi n (+ k 0.5)) len)))))))
	(loop (+ k 1)))))))

; You can convert sample vectors to spectrum vectors by way of the
; discrete cosine transform.

(define (samples->spectrum/cos sampvec)
  (let ((freq (sample-vector-sampling-frequency sampvec)))
    (make-spectrum-vector
     (discrete-cosine-transform
      (sample-vector-underlying-f32vector sampvec))
     freq)))

(define (spectrum->samples/cos specvec)
  (let ((freq (spectrum-vector-sampling-frequency specvec)))
    (make-sample-vector
     (inverse-discrete-cosine-transform
      (spectrum-vector-underlying-f32vector specvec))
     freq)))

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

; You can perform basic math on signals, such as adding them or multiplying
; them. You can also multiply a signal by a scalar value.

(define (sig+ f1 f2)
  (lambda (t)
    (+ (f1 t)
       (f2 t))))

(define (sig* f1 f2)
  (lambda (t)
    (* (f1 t)
       (f2 t))))

(define (sig-scale f1 c)
  (lambda (t)
    (* (f1 t)
       c)))

; You can also scale signals in the other direction: the time direction.

(define (time-scale f1 c)
  (lambda (t)
    (f1 (* c t))))

; You can shift signals in time. Signals which are time-shifted forward are
; left-padded with silence.

(define (time-shift f offset)
  (lambda (t)
    (if (< t offset)
	0.0
	(f (- t offset)))))

; Here are some basic signals: oscillators which produce _ideal_ sine, square,
; sawtooth and triangle waves. The square, sawtooth, and triangle oscillators
; are completely fine for LFO purposes; but are not appropriate for use in
; tone generation because they are ideal and so have infinite harmonics. When
; sampled they will lead to aliasing. Later we will introduce oscillators which
; are more appropriate for use in tone generation.

(define (sine-oscillator freq ampl phase)

    (lambda (t)
      (* ampl (sin (+ phase (ang-freq (* freq t)))))))


(define (ideal-square-oscillator freq ampl duty)

    (lambda (t)
      (let* ((t2 (* t freq))
	     (r (- t2 (floor t2))))
	(* ampl
	   (if (< r duty)
	       1.0
	       -1.0)))))

(define (ideal-saw-oscillator freq ampl)
  (lambda (t)
    (let* ((t2 (* freq t)))
      (* 2 ampl
	 (- t2 (floor (+ 0.5 t2)))))))

(define (ideal-triangle-oscillator freq ampl)
  (lambda (t)
    (let ((t (* t freq)))
      (* ampl
	 (- 1 (* 4 (let ((r (+ t 0.25)))
		  (abs (- 0.5 (- r (truncate r)))))))))))

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
4     (sig-scale f (+ 0.5 fac2)))))

; A sample-vector is a vector of samples from a signal over some arbitrary
; interval, accompanied by a sampling frequency (in a whole number of hertz).
; These samples represent some portion of a signal in the time domain. They are
; a more concrete representation of a digital signal; i.e., they are closer to
; your computer soundcard's representation of a signal as a sequence of sample
; values. They are thus also amenable to DSP techniques such as
; discrete frequency-domain transforms.

; Samples in a sample-vector are 32-bit floating point, signed, zero-centered,
; and unit-normalized. That means they range from -1.0 to 1.0 with zero in the
; middle. They are stored in an underlying SRFI 4 f32vector.

(define-record-type :sample-vector
  (really-make-sample-vector vec freq)
  sample-vector?
  (vec sample-vector-underlying-f32vector)
  (freq sample-vector-sampling-frequency))

(define (sample-vector-ref svec n)
  (f32vector-ref (sample-vector-underlying-f32vector svec) n))

(define (sample-vector-length svec)
  (f32vector-length (sample-vector-underlying-f32vector svec)))


; A spectrum-vector is like a sample-vector, but in the frequency domain.
; The sampling frequency represents the Nyquist frequency of the
; resulting spectrum (twice the max frequency in the spectrum).

(define-record-type :spectrum-vector
  (really-make-spectrum-vector vec freq)
  spectrum-vector?
  (vec spectrum-vector-underlying-f32vector)
  (freq spectrum-vector-sampling-frequency))

(define (spectrum-vector-ref svec n)
  (f32vector-ref (spectrum-vector-underlying-f32vector svec) n))

(define (spectrum-vector-length svec)
  (f32vector-length (spectrum-vector-underlying-f32vector svec)))

; Procedures to convert between the time and frequency domains using the
; discrete cosine transform types II and III. (Sometimes called DCT and inverse
; DCT.) These are strictly real-valued variations of the more general discrete
; Fourier transform.

; These implementations are both naïve and literal-minded. They are bound to be
; replaced with something more optimized in the future.

; The public Valkyree APIs for handling time and frequency domain conversions
; are in the samples->spectrum/* functions below.

(define (discrete-cosine-transform data-f32vector)
  (let* ((len (f32vector-length data-f32vector))
	 (dct-f32vector (make-f32vector len)))
    (let loop ((k 0))
      (cond
       ((>= k len) dct-f32vector)
       (else
	(f32vector-set! dct-f32vector k
			(sigma 0 (- len 1)
			       (lambda (n)
				 (*
				  (f32vector-ref data-f32vector n)
				  (cos (/ (* pi (+ n 0.5) k) len))))))
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
	 (/ (* 2 (+ (* 0.5 (f32vector-ref dct-f32vector 0))
		    (sigma 1 (- len 1)
			   (lambda (n)
			     (*
			      (f32vector-ref dct-f32vector n)
			      (cos (/ (* pi n (+ k 0.5)) len)))))))
	    len))
	(loop (+ k 1)))))))

; You can convert sample vectors to spectrum vectors by way of the
; discrete cosine transform.

(define (samples->spectrum/cos sampvec)
  (let ((freq (sample-vector-frequency sampvec)))
    (really-make-spectrum-vector
     (discrete-cosine-transform
      (sample-vector-underlying-f32vector sampvec))
     freq)))

(define (spectrum->samples/cos specvec)
  (let ((freq (spectrum-vector-frequency specvec)))
    (really-make-sample-vector
     (inverse-discrete-cosine-transform
      (spectrum-vector-underlying-f32vector specvec))
     freq)))


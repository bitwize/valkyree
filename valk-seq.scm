;; A "dollar" is 100 cents, or 1 halftone on the Western diatonic
;; scale (the tonal distance between two adjacent keys on the piano
;; keyboard, counting both black and white keys). It corresponds to a
;; multiplication factor of 2^(1/12).

(define dollar-multiplier (expt 2 (/ 1.0 12.0)))

; Generate a note table with concert A (440.00 Hz) = key 69.

(define note-table
  (let ((a 440.0))
    (let loop
	((c 0) (v (make-vector 128 0.0)))
      (cond ((>= c 128) v)
	    (else (vector-set! v c (* a (expt dollar-multiplier (- c 69))))
		  (loop (+ c 1) v))))))

;; Calculate the note length (in seconds) given a length in beats and
;; a tempo in bpm.

(define (note-length beats bpm)
  (* beats (/ 60.0 bpm)))

;; Establish a parameter for the current bpm and initialize it to 120.

(define current-bpm
  (make-parameter 120))

(define-record-type :vinst
  (make-vinst signal base-freq envelope)
  vinst?
  (signal vinst-signal)
  (base-freq vinst-base-freq)
  (envelope vinst-envelope))

(define (basic-env length)
  (sig-switch (constantly 1.0) silence length))

(define-record-type :adsr
  (make-adsr a d s r)
  adsr?
  (a adsr-attack)
  (d adsr-decay)
  (s adsr-sustain)
  (r adsr-release))

(define (gen-adsr-envelope adsr)
  (lambda (length)
    (let ((a (adsr-attack adsr))
	  (d (adsr-decay adsr))
	  (s (min 1.0 (adsr-sustain adsr)))
	  (r (adsr-release adsr)))
      (sig-switch
       (ramp 0.0 1.0 a)
       (sig-switch
	(time-delay (ramp 1.0 s d) a)
	(sig-switch
	 (constantly s)
	 (time-delay (ramp s 0.0 r) length)
	 length)
	(+ a d))
       a))))

(define (make-simple-inst f base-freq)
  (make-vinst f base-freq
	      (lambda (length) (constantly 1.0))))

(define (make-adsr-inst f base-freq aenv)    
  (make-vinst f base-freq
	      (gen-adsr-envelope aenv)))


(define-record-type :vevent
  (make-vevent time duration type param)
  vevent?
  (time vevent-time)
  (duration vevent-duration)
  (type vevent-type)
  (param vevent-param))

(define (add-event! els evt)
  (cond
   ((null? els)
    (list evt))
   ((> (vevent-time (car els))
       (vevent-time evt))
    (cons evt els))
   (else
    (let loop
	((l els))
      (cond
       ((null? (cdr l))
	(set-cdr! l (cons evt '()))
	els)
       ((> (vevent-time (cadr l))
	   (vevent-time evt))
	(set-cdr! l (cons evt (cdr l)))
	els)
       (else (loop (cdr l))))))))


(define (play-tone1 inst freq vel bstart blen bpm)
  (let* ((len (note-length blen bpm))
	 (f (sig* (pitch-modulate>>
		   (vinst-signal inst)
		   (constantly (/ freq (vinst-base-freq inst))))
		  (sig* ((vinst-envelope inst) len)
			(constantly vel)))))
    (time-delay
     f
     (note-length bstart bpm))))

(define (play-tone inst freq vel bstart blen)
  (play-tone1 inst freq vel bstart blen (current-bpm)))

(define (play-roll inst notelist start)
  (let loop ((l notelist)
	     (f silence)
	     (m start))
    (if
     (null? l)
     f
     (let* ((n (caar l))
	    (v (cadr (car l)))
	    (d (caddr (car l))))
       (loop (cdr l)  
	     (if (zero? n)
		 f
		 (sig+ f
		      (play-tone inst
				 n v m d)))
	     (+ m d))))))

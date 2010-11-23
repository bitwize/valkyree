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

(define notename-table
  (list->table
   '((c0 . 12)
     (c+0 . 13)
     (d-0 . 13)
     (d0 . 14)
     (d+0 . 15)
     (e-0 . 15)
     (e0 . 16)
     (f0 . 17)
     (f+0 . 18)
     (g-0 . 18)
     (g0 . 19)
     (g+0 . 20)
     (a-0 . 20)
     (a0 . 21)
     (a+0 . 22)
     (b-0 . 22)
     (b0 . 23)
     (c1 . 24)
     (c+1 . 25)
     (d-1 . 25)
     (d1 . 26)
     (d+1 . 27)
     (e-1 . 27)
     (e1 . 28)
     (f1 . 29)
     (f+1 . 30)
     (g-1 . 30)
     (g1 . 31)
     (g+1 . 32)
     (a-1 . 32)
     (a1 . 33)
     (a+1 . 34)
     (b-1 . 34)
     (b1 . 35)
     (c2 . 36)
     (c+2 . 37)
     (d-2 . 37)
     (d2 . 38)
     (d+2 . 39)
     (e-2 . 39)
     (e2 . 40)
     (f2 . 41)
     (f+2 . 42)
     (g-2 . 42)
     (g2 . 43)
     (g+2 . 44)
     (a-2 . 44)
     (a2 . 45)
     (a+2 . 46)
     (b-2 . 46)
     (b2 . 47)
     (c3 . 48)
     (c+3 . 49)
     (d-3 . 49)
     (d3 . 50)
     (d+3 . 51)
     (e-3 . 51)
     (e3 . 52)
     (f3 . 53)
     (f+3 . 54)
     (g-3 . 54)
     (g3 . 55)
     (g+3 . 56)
     (a-3 . 56)
     (a3 . 57)
     (a+3 . 58)
     (b-3 . 58)
     (b3 . 59)
     (c4 . 60)
     (c+4 . 61)
     (d-4 . 61)
     (d4 . 62)
     (d+4 . 63)
     (e-4 . 63)
     (e4 . 64)
     (f4 . 65)
     (f+4 . 66)
     (g-4 . 66)
     (g4 . 67)
     (g+4 . 68)
     (a-4 . 68)
     (a4 . 69)
     (a+4 . 70)
     (b-4 . 70)
     (b4 . 71)
     (c5 . 72)
     (c+5 . 73)
     (d-5 . 73)
     (d5 . 74)
     (d+5 . 75)
     (e-5 . 75)
     (e5 . 76)
     (f5 . 77)
     (f+5 . 78)
     (g-5 . 78)
     (g5 . 79)
     (g+5 . 80)
     (a-5 . 80)
     (a5 . 81)
     (a+5 . 82)
     (b-5 . 82)
     (b5 . 83)
     (c6 . 84)
     (c+6 . 85)
     (d-6 . 85)
     (d6 . 86)
     (d+6 . 87)
     (e-6 . 87)
     (e6 . 88)
     (f6 . 89)
     (f+6 . 90)
     (g-6 . 90)
     (g6 . 91)
     (g+6 . 92)
     (a-6 . 92)
     (a6 . 93)
     (a+6 . 94)
     (b-6 . 94)
     (b6 . 95)
     (c7 . 96)
     (c+7 . 97)
     (d-7 . 97)
     (d7 . 98)
     (d+7 . 99)
     (e-7 . 99)
     (e7 . 100)
     (f7 . 101)
     (f+7 . 102)
     (g-7 . 102)
     (g7 . 103)
     (g+7 . 104)
     (a-7 . 104)
     (a7 . 105)
     (a+7 . 106)
     (b-7 . 106)
     (b7 . 107)
     (c8 . 108)
     (c+8 . 109)
     (d-8 . 109)
     (d8 . 110)
     (d+8 . 111)
     (e-8 . 111)
     (e8 . 112)
     (f8 . 113)
     (f+8 . 114)
     (g-8 . 114)
     (g8 . 115)
     (g+8 . 116)
     (a-8 . 116)
     (a8 . 117)
     (a+8 . 118)
     (b-8 . 118)
     (b8 . 119))))


;; Calculate the note length (in seconds) given a length in beats and
;; a tempo in bpm.

(define (note-length beats bpm)
  (* beats (/ 60.0 bpm)))

;; Establish a parameter for the current bpm and initialize it to 120.

(define current-bpm
  (make-parameter 120))

(define current-gap-time
  (make-parameter 0.125))

(define-record-type :vinst
  (make-vinst tag signal-or-selector base-freq envelope)
  vinst?
  (tag vinst-tag)
  (signal-or-selector vinst-signal-or-selector)
  (base-freq vinst-base-freq)
  (envelope vinst-envelope))

(define (vinst-signal inst freq vel)
  (cond
   ((eq? (vinst-tag inst) 'fixed) (vinst-signal-or-selector inst))
   ((eq? (vinst-tag inst) 'variable) ((vinst-signal-or-selector inst) freq vel))
   (else (error "invalid tag in instrument" vinst-signal))))

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
	 (sig-switch
	  (time-delay (ramp s 0.0 r) length)
	  silence
	  (+ length r))
	 length)
	(+ a d))
       a))))

(define (make-simple-inst f base-freq)
  (make-vinst 'fixed f base-freq
	      (lambda (length) (sig-switch (constantly 1.0) silence length))))

(define (make-adsr-inst f base-freq aenv)    
  (make-vinst 'fixed f base-freq
	      (gen-adsr-envelope aenv)))

(define (make-simple-variable-inst selector base-freq)
  (make-vinst 'variable selector base-freq
	      (lambda (length) (sig-switch (constantly 1.0) silence length))))

(define (make-adsr-variable-inst selector base-freq aenv)    
  (make-vinst 'variable selector base-freq
	      (gen-adsr-envelope aenv)))



(define-record-type :vevent
  (make-vevent time duration type param)
  vevent?
  (time vevent-time)
  (duration vevent-duration)
  (type vevent-type)
  (param vevent-param))

(define (raw-vevent-note-frequency evt)
  (car (vevent-param evt)))

(define (raw-vevent-note-velocity evt)
  (cdr (vevent-param evt)))

(define (assert-vevent-type event type)
  (if (not (eq? (vevent-type event) type))
      (error "Wrong event type " (vevent-type event))))
      

(define (vevent-note-frequency evt)
  (assert-vevent-type evt 'note)
  (raw-vevent-note-frequency evt))

(define (vevent-note-velocity evt)
  (assert-vevent-type evt 'note)
  (raw-vevent-note-velocity evt))

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


(define (play-note>> inst freq vel start len)
  (let* ((f (sig* (pitch-modulate>>
		   (vinst-signal inst freq vel)
		   (constantly (/ freq (vinst-base-freq inst))))
		  (sig* ((vinst-envelope inst) len)
			(constantly vel)))))
    (time-delay
     f
     start)))

(define (play-events>> inst eventlist)
  (let loop ((l eventlist)
	     (f silence))
    (if
     (null? l)
     f
     (loop (cdr l)  
	   (if (not  (eq? (vevent-type (car l)) 'note))
	       f
	       (let* ((n (car l))
		      (q (car (vevent-param n)))
		      (v (cdr (vevent-param n)))
		      (t (vevent-time n))
		      (d (vevent-duration n)))
		 
		 (sig-switch
		  f
		  (play-note>> inst
			       q v t d)
		  t))
	     )))))

(define (note-number-or-name->freq number-or-name)
  (vector-ref note-table
	     (if
	      (symbol? number-or-name)
	      (table-ref notename-table number-or-name)
	      number-or-name)))

;; notespec->event : list float -> vevent

;;   Changes the notespec given by `n' into a note event starting at
;;   `start'. A notespec is a list of the following form:

;;      (note-name strength length)

;;   where `strength' is a relative value of how hard to hit or how
;;   loud to play the note (1.0 = full) and `length' is in beats. For
;;   example, playing middle C at half strength for a quarter note in
;;   4/4 time would have this notespec:

;;      (c4 0.5 1.0)

(define (notespec->event n start)
  (make-vevent start 
	       (note-length (max 0.0 (- (caddr n) (current-gap-time)))
			    (current-bpm))
	       'note
	       (cons (note-number-or-name->freq (car n)) (cadr n))))


(define (roll->events notelist)
  (let loop
      ((start 0.0)
       (nl notelist)
       (el '()))
    (cond
     ((null? nl) (reverse el))
     ((eq? (car (car nl)) 'rest) (loop (+ start (note-length (cadr (car nl)) (current-bpm))) (cdr nl) el))
     (else (let ((ev (notespec->event (car nl) start)))
	     (loop (+ start (note-length (caddr (car nl)) (current-bpm))) (cdr nl) (cons ev el)))))))

(define (tweak-freqs eventlist sample-freq)
  (map (lambda (event)
	 (if (eq? (vevent-type event) 'note)
	     (let* ((orig-f (raw-vevent-note-frequency event))
		    (divisor (round (/ sample-freq orig-f)))
		    (new-f (/ sample-freq divisor)))
	       (make-vevent
		(vevent-time event)
		(vevent-duration event)
		(vevent-type event)
		(cons new-f (raw-vevent-note-velocity event))))
	     event))
       eventlist))
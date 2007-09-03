(define *htf* (expt 2 (/ 1.0 12.0)))

(define *note-table*
  (let* ((a 220))
    (let loop
	((c -12) (v (make-vector 60 0.0)))
      (cond ((>= c 48) v)
	    (#t (vector-set! v (+ c 12) (* a (expt *htf* c)))
		(loop (+ c 1) v)
		)))))


(define (note-length beats bpm)
  (* beats (/ 60.0 bpm)))

(define current-bpm
  (make-parameter 120))

(define (make-simple-inst f . specifier)
  (let* ((nl
	  (if 
	   (null? specifier) 0.875
	   (case (car specifier)
	     ((legato)
	      1.0)
	     ((staccato)
	      0.5)
	     (else 0.875))
	   
	   )))
    (lambda (freq vel len)
      (let* ((f2 (f freq vel)) (len2 (* len nl)))
	(lambda (i)
	  (if (> i len2) 0 (f2 i)))))))

(define (make-adsr-inst f aenv . specifier)
  (let* ((nl
	  (if
	   (null? specifier) 0.875
	   (case (car specifier)
	     ((legato)
	      1.0)
	     ((staccato)
	      0.5)
	     (else 0.875)))))
    
    (lambda (freq vel len)
      (let* ((f2 (f freq vel)) (len2 (* len nl)))
	(sig*
	 f2
	 (adsr-envelope-gen aenv len2))))))

(define-record-type :vtrack
  (make-vtrack qps elist)
  vtrack?
  (qps vtrack-qps)
  (elist vtrack-elist))

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
  (let* (
	 (len (note-length blen bpm))
	 (f (inst freq vel len))
	 )
    (sample-offset
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
		 (mix f
		      (play-tone inst
				 n v m d)))
	     (+ m d))))))

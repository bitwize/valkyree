(define *htf* (expt 2 1/12))

(define *note-table*
  (let* (
	 (a 220)
	 )
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
	  (cond
	   ((null? specifier) 0.875)
	   ((eq? (car specifier) 'legato) 1.0)
	   ((eq? (car specifier) 'staccato) 0.5))))
    (lambda (freq vel len)
      (let* ((f2 (f freq vel)) (len2 (* len nl)))
	(lambda (i)
	  (if (> i len2) 0 (f2 i)))))))

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
	     (mix f (if (zero? n)
			silence
			(play-tone inst
				   n v m d)))
	     (+ m d))))))

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
		)
	    )
      )
    )
  )

(define (note-length beats bpm)
  (round (* beats (valkyree-freq 'samples) (/ 60 bpm)))
  )

(define (make-simple-inst f . specifier)
  (let* ((nl
	  (cond
	   ((null? specifier) 7/8)
	   ((eq? specifier 'legato) 1)
	   ((eq? specifier 'staccato) 1/2)
	   )))
    (lambda (freq len)
      (let* ((f2 (f freq)) (len2 (round (* len nl))))
	(lambda (i)
	  (if (> i len2) 0 (f2 i)))))))

(define (play-tone inst freq bstart blen bpm)
  (let* (
	 (len (note-length blen bpm))
	 (f (inst freq len))
	 )
    (sample-offset-gen
     f
     (note-length bstart bpm))))

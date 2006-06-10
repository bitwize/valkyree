(define *chopsticks-note-table1*
  (apply append
	 (map (lambda (x)
		(list `(,x 0.25 0.333)
		      '(0 0 0.333)))
	      '(392 392 392 392 392 392 392 392 392 392 392 392
		    494 494 494 494 440 494 523 523 523 523))))


(define *chopsticks-note-table2*
  (apply append
	 (map (lambda (x)
		(list `(,x 0.25 0.333)
		      '(0 0 0.333)))
	      '(349 349 349 349 349 349 330 330 330 330 330 330
		    294 294 294 294 262 294 262 262 262 262))))

(define *inst* (make-simple-inst
		 square-wave
		))

(write-raw "chopsticks.raw"
	   (mix
	    (play-roll *inst* *chopsticks-note-table1* 0)
	    (play-roll *inst* *chopsticks-note-table2* 0))
	   8.0 22050)


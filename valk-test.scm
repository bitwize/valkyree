;(include "valk-snd.scm")


;Two simple tests.

(define scale-test
  (mix-gen
   (map
    (lambda (q d)
      (sample-offset-gen 
       (lambda (i)
	 (if (> i 22050) 0 (sine-wave i q)))       
       (* 22050 d)))   
    '(262 294 330 349 392 440 494 523)
    '(0 1 2 3 4 5 6 7))))

(define chord-test
  (mix-gen
   (map (lambda (f)
	  (change-ampl-gen f 0.33))
	(list
	 (square-gen 262)
	 (square-gen 330)
	 (square-gen 392)))))

(write-raw "scale.raw" scale-test 220500)
(write-raw "chord.raw" chord-test 44100)

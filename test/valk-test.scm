(define tolerance (make-parameter 0.0001))

(define (close-enough? estimate value)
  (< (abs (- value estimate)) (tolerance)))

(define reset-test-counters! #f)
(define increment-tests-run! #f)
(define increment-tests-passed! #f)
(define tests-run #f)
(define tests-passed #f)

(let ((tr (make-parameter 0))
      (tp (make-parameter 0)))
  (set! reset-test-counters! (lambda () (tr 0) (tp 0)))
  (set! increment-tests-run! (lambda () (tr (+ (tr) 1))))
  (set! increment-tests-passed! (lambda () (tp (+ (tp) 1))))
  (set! tests-run (lambda () (tr)))
  (set! tests-passed (lambda () (tp))))


(define (run-test-thunk docstring thunk)
  (increment-tests-run!)
  (display "checking ")
  (display docstring)
  (display " -- ")
  (display (if (thunk) (begin (increment-tests-passed!) "yes") 
	       "no"))
  (newline))

(define-macro (run-test docstring . body)
  `(run-test-thunk ,docstring (lambda () ,@body)))

(define (show-results)
  (display (tests-passed))
  (display " of ")
  (display (tests-run))
  (display " tests passed")
  (newline)
  (newline)
  (if (< (tests-passed) (tests-run))
      (begin (display "You have some bugs; please fix them!")
	     (newline))))

(reset-test-counters!)

(run-test "test framework" #t)

(let ((co (constantly 0.5)))
  (run-test "constantly works"
	    (and
	     (close-enough? 0.5 (co 0.0))
	     (close-enough? 0.5 (co 0.5))
	     (close-enough? 0.5 (co 1.0))
	     (close-enough? 0.5 (co 1024.0)))))

(let ((so (sine-oscillator 1.0 1.0 0.0)))
  (run-test "sine-oscillator works"
	    (and
	     (close-enough? 0.0 (so 0.0))
	     (close-enough? 0.7071067 (so 0.125))
	     (close-enough? 1.0 (so 0.25))
	     (close-enough? 0.7071067 (so 0.375))
	     (close-enough? 0.0 (so 0.5))
	     (close-enough? -0.7071067 (so 0.625))
	     (close-enough? -1.0 (so 0.75))
	     (close-enough? -0.7071067 (so 0.875)))))

(let ((qo (ideal-square-oscillator 1.0 1.0 0.5)))
  (run-test "ideal-square-oscillator works"
	    (and
	     (close-enough? 1.0 (qo 0.0))
	     (close-enough? 1.0 (qo 0.2))
	     (close-enough? 1.0 (qo 0.4))
	     (close-enough? -1.0 (qo 0.6))
	     (close-enough? -1.0 (qo 0.8)))))

(let ((wo (ideal-saw-oscillator 1.0 1.0)))
  (run-test "ideal-saw-oscillator works"
	    (and
	     (close-enough? 0.0 (wo 0.0))
	     (close-enough? 0.25 (wo 0.125))
	     (close-enough? 0.5 (wo 0.25))
	     (close-enough? 0.75 (wo 0.375))
	     (close-enough? 1.0 (wo 0.499999))
	     (close-enough? -1.0 (wo 0.500001))
	     (close-enough? -0.75 (wo 0.625))
	     (close-enough? -0.5 (wo 0.75))
	     (close-enough? -0.25 (wo 0.875)))))

(let ((to (ideal-triangle-oscillator 1.0 1.0)))
  (run-test
   "ideal-triangle-oscillator works"
   (and
    (close-enough? 0.0 (to 0.0))
    (close-enough? 0.5 (to 0.125))
    (close-enough? 1.0 (to 0.25))
    (close-enough? 0.5 (to 0.375))
    (close-enough? 0.0 (to 0.5))
    (close-enough? -0.5 (to 0.625))
    (close-enough? -1.0 (to 0.75))
    (close-enough? -0.5 (to 0.875))
    (close-enough? 0.0 (to 1.0)))))

(let* ((so (sine-oscillator 1.0 0.5 0.0))
      (f  (constantly 0.25))
      (no (sig+ so f)))
  (run-test
   "signal addition works"
   (and
    (close-enough? 0.25 (no 0.0))
    (close-enough? 0.75 (no 0.25))
    (close-enough? 0.25 (no 0.5))
    (close-enough? -0.25 (no 0.75)))))

(let* ((so (sine-oscillator 1.0 0.5 0.0))
       (f  (constantly 2.0))
       (no (sig* so f)))
  (run-test
   "signal multiplication works"
   (and
    (close-enough? 0.0 (no 0.0))
    (close-enough? 1.0 (no 0.25))
    (close-enough? 0.0 (no 0.5))
    (close-enough? -1.0 (no 0.75)))))

(let* ((so (sine-oscillator 1.0 0.5 0.0))
       
       (no (sig-scale so 2.0)))
  (run-test
   "signal scalar multiplication works"
   (and
    (close-enough? 0.0 (no 0.0))
    (close-enough? 1.0 (no 0.25))
    (close-enough? 0.0 (no 0.5))
    (close-enough? -1.0 (no 0.75)))))

(let* ((so (sine-oscillator 1.0 1.0 0.0))
       (sv (make-sample-vector so 1.0 32)))
  (run-test
   "sample-vectors work"
   (and
    (= (sample-vector-length sv) 32)
    (let loop
	((i 0))
      (cond
       ((>= i 32) #t)
       ((not (close-enough? (sample-vector-ref sv i)
			    (sin (* twopi (/ i 32))))) #f)
       (else (loop (+ i 1))))))))
  
(show-results)
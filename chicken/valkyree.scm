(require-extension lolevel)
(require-extension srfi-4)
(require-extension srfi-9)


(declare (unit valkyree))



(define (write-byte v port)
  (display (integer->char v) port))

(define (read-byte port)
  (char->integer (read-char port)))


(define (u8vector-append . vs)
  (let* ((total-length (apply + (map u8vector-length vs)))
	 (new-vector (make-u8vector total-length 0)))
    (let loop ((l vs)
	       (start 0))
      (if (null? l) new-vector
	  (let loop2 ((i 0))
	    (if (>= i (u8vector-length (car l)))
		(loop (cdr l)
		      (+ start (u8vector-length (car l))))
		(begin (u8vector-set! new-vector
				      (+ start i)
				      (u8vector-ref (car l) i))
		       (loop2 (+ i 1)))))))))

; THIS IS A HACK.
; Because Chicken lacks proper octet block I/O.

(define (read-subu8vector v start end port)
  (if (< end start)
      (error "end of subu8vector must come after start")
      (let loop
	  ((i start)
	   (b (read-byte port)))
	
	(cond
	 ((or
	   (>= i end)
	   (eof-object? b))
	  
	  (- i start))
	 
	 (else
	  (u8vector-set! v i b)
	  (loop (+ i 1)
		(read-byte port)))))))

(define (write-subu8vector v start end port)
  (if (< end start)
      (error "end of subu8vector must come after start")
      (let loop
	  ((i start))
	(cond
	 ((>= i end)
	  (- end start))
	 (else
	  (write-byte (u8vector-ref v i)
		      port)
	  (loop (+ i 1)))))))

(include "../common/valk-snd.scm")
(include "../common/valk-seq.scm")
(include "../common/valk-file.scm")

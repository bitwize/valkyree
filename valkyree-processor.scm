#!/usr/bin/env gsi-script

(define valkyree-home (getenv "VALKYREE_HOME"))

(define cl (command-line))

(define (load-relative path file)
  (define (path-append path file)
    (cond ((string=? path "") file)
	  ((char=? (string-ref 
		    path
		    (- (string-length path) 1)) #\/)
	   (string-append path file))
	  (else (string-append path "/" file))))
  (load (path-append path file)))

(load-relative valkyree-home "valkyree-gambit")

(if (null? (cdr cl))
    (error "please give me a file to process"))

(define (truncate-ext path)
  (let loop ((i (- (string-length path) 1)))
      (cond
       ((< i 0) path)
       ((char=? (string-ref path i) #\.)
	(substring path 0 i))
       (else (loop (- i 1))))))

(let* ((file (cadr cl))
       (target (string-append (truncate-ext file) ".wav")))
  (current-sound-output (make-wav-file-emitter target))
  (load file))
(require-extension lolevel)
(require-extension srfi-4)

(declare (unit valkyree))

(define (write-byte v port)
  (display (integer->char v) port))

; THIS IS A HACK.
; Because Chicken lacks proper octet block I/O.

(define (write-subu8vector v start end port)
  (let loop
      ((i start))
    (cond
     ((>= i end)
      (if #f #f))
     (else
      (write-byte (u8vector-ref v i)
		  port)
      (loop (+ i 1))))))

(include "../common/valk-snd.scm")
(include "../common/valk-seq.scm")
(include "../common/valk-file.scm")

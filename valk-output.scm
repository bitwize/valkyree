(define current-sound-output
  (make-parameter (lambda x #f)))
(define current-stereo-sound-output
  (make-parameter (lambda x #f)))

(define (output-sound f duration)
  ((current-sound-output) f duration))

(define (output-stereo-sound f duration)
  ((current-stereo-sound-output) f duration))

(define (make-wav-file-emitter fn . opt-freq)
  (lambda (f dur)
    (write-wav fn f dur (if (null? opt-freq) 44100 (car opt-freq)))))

(define (make-stereo-wav-file-emitter fn . opt-freq)
  (lambda (f dur)
    (stereo-write-wav fn f dur (if (null? opt-freq) 44100 (car opt-freq)))))

(define (make-ascii-wavetable-emitter fn . opt-freq)
  (lambda (f dur)
    (write-ascii-wavetable fn f dur (if (null? opt-freq) 44100 (car opt-freq)))))
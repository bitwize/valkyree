
(define-module valkyree
  (export constantly sine-wave square-wave saw-wave envelope-ampl change-ampl
	  silence
	  make-adsr-envelope adsr-attack adsr-decay adsr-sustain adsr-release
	  adsr-envelope-fun make-adsr-inst
	  sample-offset mix stereo-mix stereo mono->stereo pan
	  sound-render-s16vector sound-render-s16vector-st
	  sound-render-f32vector sound-render-f32vector-st
	  write-raw st-write-raw
	  note-length current-bpm
	  sampled-sound-u16
	  sampled-sound
	  sampled-sound-looping
	  harmonics
	  make-wavetable
	  wavetable-function
	  make-simple-inst make-adsr-inst
	  play-tone play-roll))

(select-module valkyree)
(use gauche.parameter)
(use gauche.uvector)
(use srfi-9)


(define (write-subu8vector v start end port)
  (write-block v port start end)
  (- end start))

(define (read-subu8vector v start end port)
  (let* ((n (read-block v port start end)))
    (if (eof-object? n)
	0
	n)))

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

(define bitwise-and logand)
(define bitwise-ior logior)
(define arithmetic-shift ash)
(load "../common/valk-snd.scm")
(load "../common/valk-file.scm")
(load "../common/valk-seq.scm")

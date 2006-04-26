
(define-module valkyree
  (export constantly sine-wave square-wave envelope-ampl change-ampl
	  make-adsr-envelope adsr-attack adsr-decay adsr-sustain adsr-release
	  adsr-envelope-fun make-adsr-inst
	  sample-offset mix stereo-mix stereo mono->stereo pan
	  sound-render-s16vector write-raw st-write-raw
	  note-length current-bpm make-simple-inst play-tone
	  play-roll))

(select-module valkyree)
(use gauche.parameter)
(use gauche.uvector)
(use srfi-9)


(define (write-subu8vector v start end port)
  (write-block v port start end))
(define bitwise-and logand)
(define bitwise-ior logior)
(define arithmetic-shift ash)
(load "../common/valk-snd.scm")
(load "../common/valk-file.scm")
(load "../common/valk-seq.scm")

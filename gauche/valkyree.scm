
(define-module valkyree
  (export constantly sine-wave square-wave envelope-ampl change-ampl
	  sample-offset mix stereo-mix stereo mono->stereo pan
	  sound-render-s16vector write-raw st-write-raw
	  note-length current-bpm make-simple-inst play-tone
	  play-roll))

(select-module valkyree)
(use gauche.parameter)
(use gauche.uvector)

(define (write-subu8vector v start end port)
  (write-block v port start end))

(load "../common/valk-snd.scm")
(load "../common/valk-file.scm")
(load "../common/valk-seq.scm")
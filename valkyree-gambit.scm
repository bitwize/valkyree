(declare (standard-bindings))
(declare (extended-bindings))
(declare (not safe))
(define write-byte write-u8)
(define read-byte read-u8)
(define char->ascii char->integer)
(define ascii->char integer->char)
(define flonum->fixnum ##flonum.->fixnum)
(declare (fixnum))
(include "srfi-1-gambit.scm")
(declare (generic) (mostly-fixnum-flonum))
(include "valk-num.scm")
(include "valk-snd.scm")
(include "valk-seq.scm")
(include "valk-render.scm")
(include "valk-riff.scm")
(include "valk-file.scm")


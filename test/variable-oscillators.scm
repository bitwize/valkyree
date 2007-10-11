(load "../valkyree-gambit.scm")

(define (quantize sig interval)
  (lambda (t)
    (sig (* interval (truncate (/ t interval))))))

(define pitch-modulator (lambda (t) (let ((t2 (* t 20))) (/ 1 (+ 1 (- t2 (* 10 (truncate (/ t2 10)))))))))

(define output-waveform (pitch-modulate (sine-oscillator 262.00
					 0.25
					 0)
					pitch-modulator))

(write-wav "var.wav" output-waveform
	   5.0 22050)
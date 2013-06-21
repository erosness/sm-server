(use srfi-4)

(define (biquad-coefficients gain f0 Fs Q)
  (let ((buffer (make-f64vector 5)))
    ((foreign-lambda int "fill_biquad_coefficients"
                f64vector
                double ;; gain
                double ;; f0
                double ;; Fs
                double ;; Q
                )
     buffer gain f0 Fs Q)
    buffer))

;; usage:
;;                      db   hz  samples/s   ?
;; (biquad-coefficients  1  150  44100      0.7)
;; returns a f64vector of 5 elements b0 b1 b2 a1 a2

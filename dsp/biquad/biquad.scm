(module biquad *

(import chicken scheme foreign)
(use srfi-4)

(define (biquad-coefficients gain f0 Fs Q)
  (let ((buffer (make-f64vector 5)))
    ((foreign-lambda int "fill_biquad_coefficients"
                f64vector
                double  ;; gain
                double  ;; f0
                double  ;; Fs
                double  ;; Q
                )
     buffer gain f0 Fs Q)
    buffer))

;; the ADAU1701 sigmadsp stores a0 and a1 negated, which is what
;; the tablegen displays.
(define (invert-a-coefficients vector)
  (define (r idx) (f64vector-ref vector idx))
  (f64vector (r 0)
             (r 1)
             (r 2)
             (- (r 3))
             (- (r 4))))

(define (sigma-eq-coefficients gain f0_ samplingrate slope)
  (f64vector->list
   (invert-a-coefficients
    (biquad-coefficients gain f0_ samplingrate slope))))

;; usage:
;;                      db   hz  samples/s   ?
;; (biquad-coefficients  1  150  44100      0.7)
;; returns a f64vector of 5 elements b0 b1 b2 a1 a2
)

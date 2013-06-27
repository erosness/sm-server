(module dsp *

(import chicken scheme srfi-1 srfi-4 foreign)

(include "safeload.scm")

(use biquad q523)

(define *eq-addresses* (include "sigma-eq-addresses.scm"))
(define *band-frequencies* '(63 120 500 2000 10000)) ;; hz

(define (eq-packets band-index gain
                    #!optional
                    (hz (list-ref *band-frequencies* band-index))
                    (samplerate 48000)
                    (slope 0.71))
  ;; band-index is 0-4 and addresses which of the 5 eq-sliders we're
  ;; chaning. we need this index for register addresses and hz for
  ;; coefficient calculations.
  (assert (< band-index 5))

  ;; fxpoint forms of our 5 coefficient parameters
  (define fxcs (map fp->q523 (sigma-eq-coefficients gain hz samplerate slope)))

  ;; parameter address of band component
  (define band-addresses (list-ref *eq-addresses* band-index))

  (safeload
   (map (lambda (cfx idx)
          (cons (list-ref band-addresses idx)
                (u8vector->blob/shared cfx)))
        fxcs
        (iota 5))))


)

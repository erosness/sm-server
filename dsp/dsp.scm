(module dsp *

(import chicken scheme srfi-1 srfi-4 foreign)

(include "safeload.scm")

(use biquad q523 posix data-structures)

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

(define (volume-packets volume)
  (let ((vol523 (blob->string (u8vector->blob (fp->q523 volume)))))
    ;; TODO: get these param addresses from sigma exports
    (safeload `((#x4B . ,vol523)
                (#x4C . "\x00\x00\x08\x00"))))) ;; <-- stepsize

(define (mute-packets mute?)
  (list (conc "\x00\x53\x00" (if mute? "\x00" "\x80") "\x00\x00")))

(define (i2c-writer fd packets)
  (for-each (lambda (packet) (file-write fd packet))
            packets))

(define (set-volume fd volume) (i2c-writer fd (volume-packets volume)))
(define (set-mute   fd mute?)  (i2c-writer fd (mute-packets mute?)))
(define (set-eq     fd . args) (i2c-writer fd (apply eq-packets args)))

)

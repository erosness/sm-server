(module dab-i2c-mock *

(import chicken scheme)
(use matchable gochan srfi-18)

(define mock-freq 89000)
(define (fm-frequency . hz)
  (if (null? hz)
      mock-freq
      (begin
        (set! mock-freq (car hz))
        mock-freq)))

(define (fm-radio-text)
  (match mock-freq
    (99000 "NRK P3")
    (98000 "blablabla")
    (92000 "Foobar")
    (95600 "HOHOHOHO!")
    (89100 "Test channel 2")
    (else "")))

(define (clamp-range min max)
  (lambda (x)
    (cond
     ((<= min x max) x)
     ((< x min) (- max (modulo min x)))
     (else (+ min (modulo x max))))))

(define fm-range (clamp-range 87500 101000))
(define (fm-signal-strength) -89)

(define mock-tunestatus 'decoding)
(define (fm-tunestatus) mock-tunestatus)


(define current-search 'idle)
(define (fm-search direction)
  (set! current-search direction)
  (set! mock-tunestatus (if (eq? direction 'idle) 'decoding 'idle)))

(define (hardware-tick)
  (if (not (eq? current-search 'idle))
      (let ((op (if (eq? current-search 'up) + -)))
        (set! mock-freq (fm-range (op mock-freq 100)))
        (if (not (equal? (fm-radio-text) ""))
            (begin
              (print "found station, stopping search")
              (fm-search 'idle))))))

(define (hardware chan)
  (lambda ()
    (let loop ()
      (let ((msg (gochan-receive* chan 0.2)))
        (match msg
          (#f (print "all channels closed") )
          (else (hardware-tick)))
        (if msg (loop))))))

(define c (gochan))
(define fm-hardware (hardware c))
(define hw-thread (thread-start! (make-thread fm-hardware "dummy-hw-thread")))

(define (fm-on?) #t))
(import dab-i2c-mock)

(include "rest-fm.scm")

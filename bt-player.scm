(module bt-player *

(import scheme chicken)
;; (use nanoif)

(define (bt-next turi)
  (print "At bt-next: " turi))

(define (bt-prev turi)
  (print "At bt-prev: " turi))

(define (bt-pause)
  (print "At bt-pause"))

(define (bt-unpause)
  (print "At bt-unpause"))

)

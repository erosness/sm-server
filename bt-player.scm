(module bt-player *

(import scheme chicken)
(use nanoif)

(define (make-bt-if)
  (let ((btif (make-nano-if
                "ipc:///data/nanomessage/bt.pair"
                "ipc:///data/nanomessage/bt.pub")))
    (set-handler btif (lambda (obj) (print "At push handler: "  obj )))
    btif))



(define (bt-next turi)
  (print "At bt-next: " turi))

(define (bt-prev turi)
  (print "At bt-prev: " turi))

(define (bt-pause)
  (print "At bt-pause"))

(define (bt-unpause)
  (print "At bt-unpause"))

)


(import bt-player)

(define *bt* (make-bt-if))

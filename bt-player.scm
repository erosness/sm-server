(module bt-player *

(import scheme chicken)
(use nanoif)

(define btif
   (make-nano-if
                "ipc:///data/nanomessage/bt.pair"
                "ipc:///data/nanomessage/bt.pub"))

(set-handler btif (lambda (obj) (print "At push handler: "  obj )))

(define (bt-next)
  (nano-if-request btif `("next")))

(define (bt-prev)
  (nano-if-request btif `("prevoius")))

(define (bt-pause)
(nano-if-request btif `("pause")))

(define (bt-unpause)
  (nano-if-request btif `("unpause")))

)

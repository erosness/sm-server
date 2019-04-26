(module bt-player *

(import scheme chicken)
(use nanoif)

(define btif
   (make-nano-if
                "ipc:///data/nanomessage/drv_bluetooth.pair"
                "ipc:///data/nanomessage/drv_bluetooth.pub"))

;; Mount default handler with debug output.
(set-handler btif (lambda (obj) (print "At push handler: "  obj )))

(define (bt-next)
  (nano-if-request btif `("next")))

(define (bt-prev)
  (nano-if-request btif `("prevoius")))

(define (bt-pause)
(nano-if-request btif `("pause")))

(define (bt-unpause)
  (nano-if-request btif `("unpause")))

(define (bt-start-pair)
(nano-if-request btif `("start_pair")))

(define (bt-end-pair)
  (nano-if-request btif `("end_pair")))

(define (bt-set-handler handler)
  (set-handler btif handler))

)

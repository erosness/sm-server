(module bt-player *

(import scheme chicken data-structures)

(use restlib nanoif)

(import store)

(define btif
   (make-nano-if
                "ipc:///data/nanomessage/drv_bluetooth.pair"
                "ipc:///data/nanomessage/drv_bluetooth.pub"))

;; Mount default handler with debug output.
(set-handler btif (lambda (obj) (print "At push handler: "  obj )))

(define store (make-store (string->symbol
                                   (conc "speaker-icon" "-"
                                         (rest-server-port)))))
(define my-default-name "Maestro")

(define (my-name)
  (and-let* ((icon-store (store)))
    (let ((my-given-name (alist-ref 'name (store))))
      (if my-given-name
        my-given-name
        my-default-name))))

(define (bt-next)
  (nano-if-request btif `("next")))

(define (bt-prev)
  (nano-if-request btif `("previous")))

(define (bt-pause)
(nano-if-request btif `("pause")))

(define (bt-unpause)
  (nano-if-request btif `("unpause")))

(define (bt-refresh)
  (nano-if-request btif `("refresh")))

(define (bt-start-pair)
  (nano-if-request btif `("start_pair")))

(define (bt-end-pair)
  (nano-if-request btif `("end_pair")))

(define (bt-name)
  (nano-if-request btif `("name" ,(my-name))))

(define (bt-set-handler handler)
  (set-handler btif handler))

)

;; Nano module

(module nanoif (make-nano-if)

(import extras chicken scheme srfi-1)

(use nanomsg clojurian-syntax looper srfi-18 data-structures)

(define (make-nano-socket addr)
  (let ((nnsock (nn-socket 'pair)))
    (print "Connect: " (nn-connect nnsock addr))
    nnsock))

;; Record type to handle communication with gstplay.
(define-record-type nano-if (%make-nano-if socket req-mutex handlers response)
  nano-if?
  (socket get-socket make-socket)
  (req-mutex get-mutex)
  (handlers get-handlers set-handler)
  (response get-response set-response))

(define-record-printer nano-if
  (lambda (rec out)
    (fprintf out "nano-if handlers: ~S responses:"
                  (if (get-handlers rec) "Has handler" "No handler"))))

(define (make-nano-if addr)
  (print "Begin make-nano-if")
  (%make-nano-if (make-nano-socket addr) (make-mutex) #f #f))

)

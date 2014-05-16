;;; helpers for broadcasting alerts to peers.
;;;
;;; === glossary ===
;;; broadcast: any UDP multicase/broadcast message
;;;
;;; change-message: a HTTP-like packet describing a change in the
;;; statemap

(module broadcast (change-message)

(import chicken scheme ports data-structures)
(use socket intarweb spiffy medea multicast)


(define (udp-broadcast-headers rq)
  (let ((echo (header-value 'echo (request-headers rq))))
    (if echo
        (conc "\n" "Echo: " echo)
        "")))

(define end-headers "\r\n\r\n")

;; create a message that represents `path` changing into `body`
(define (change-message path body)
  (conc "NOTIFY "
        path
        (udp-broadcast-headers (or (current-request) (make-request)))
        end-headers
        body))

;; like change-message, but used for large datasets. tbd
(define (make-alert path)
  (conc "ALERT "
        path
        (udp-broadcast-headers (or (current-request) (make-request)))
        end-headers))

(include "broadcast.tests.scm")
)

;;; helpers for broadcasting alerts to peers.
;;;
;;; === glossary ===
;;; broadcast: any UDP multicase/broadcast message
;;;
;;; change-message: a HTTP-like packet describing a change in the
;;; statemap

(module notify (change-message)

(import chicken scheme ports data-structures)
(use socket intarweb spiffy medea multicast)

;; create a message that represents `path` changing into `body`. port
;; specifies "owner" so change-message origins can be identified
;; (usually port-number of running service).
;;
;; path is string. body is medea-json. port is number.
(define (change-message path body port)
  (assert (not (string? body))) ;; <-- just to make sure we catch api
                                ;; change (no more strings - use
                                ;; alists!)
  (conc "NOTIFY "
        (json->string `((variable . ,path)
                        (owner . ((port . ,port)))
                        (data . ,body)
                        (echo . ,(cond ((current-request) =>
                                        (lambda (req)
                                          (header-value 'echo (request-headers req))))
                                       (else #f)))))))


(include "notify.tests.scm")
)

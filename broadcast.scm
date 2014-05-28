;;; helpers for broadcasting alerts to peers.
;;;
;;; === glossary ===
;;; broadcast: any UDP multicase/broadcast message
;;;
;;; change-message: a HTTP-like packet describing a change in the
;;; statemap

;;; TODO: rename this to something a little more descriptive (notifications?)
(module broadcast (change-message)

(import chicken scheme ports data-structures)
(use socket intarweb spiffy medea multicast)


(define (udp-broadcast-headers rq #!optional port)
  (let ((echo (header-value 'echo (request-headers rq))))
    (conc
     (if echo (conc "\nEcho: " echo) "")
     (if port (conc "\nPort: " (number->string port)) "")
     "\r\n\r\n")))


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

;; like change-message, but used for large datasets. tbd
(define (make-alert path)
  (conc "ALERT "
        path
        (udp-broadcast-headers (or (current-request) (make-request)))))

(include "broadcast.tests.scm")
)

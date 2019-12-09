;; Server for in-door part of the doorbell.
(module rest-doorbell-in ()

(import chicken scheme data-structures srfi-1 intarweb spiffy srfi-69 data-structures)

;; local imports
(import restlib store sm-config gpio)

(define (fid uid cap)
  (string-hash (conc uid cap)))


(define doorbell-out-store (make-store (string->symbol
                             (conc "doorbell-in" "-"
                               (rest-server-port)))))

;; Parts implemented as physical connection via GPIO
(define phy-unlock-button?  (make-gpio-input 10))
(define phy-connect-button? (make-gpio-input 12))

;; Parts implemented as SW modules

;; Common return status definition
(define (status?)
  `((fid . ,(fid (uid) "doorbell-in"))
    (unlockButton  . ,(phy-unlock-button?))
    (connectButton . ,(phy-connect-button?))))

;; The pure GET status (no PUT)
(define-handler /v1/sm/doorbell-in/status
  (lambda () (status?)))

;; PUT definitions

)

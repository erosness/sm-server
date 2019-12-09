;; Server for out-door part of the doorbell.
(module rest-doorbell-out ()

(import chicken scheme data-structures srfi-1 intarweb spiffy srfi-69 data-structures)

;; local imports
(import restlib store sm-config gpio)

(define (fid uid cap)
  (string-hash (conc uid cap)))


(define doorbell-out-store (make-store (string->symbol
                             (conc "doorbell-out" "-"
                               (rest-server-port)))))

;; Parts implemented as physical connection via GPIO
(define phy-doorbell? (make-gpio-input   3))
(define phy-unlock?   (make-gpio-input   8))
(define phy-unlock!   (make-gpio-output  7))
(define phy-dooropen? (make-gpio-input   5))

;; Parts implemented as SW modules

;; Common return status definition
(define (status?)
  `((fid . ,(fid (uid) "doorbell-out"))
    (doorbell . ,(phy-doorbell?))
    (unlock . ,(phy-unlock?))
    (dooropen . ,(phy-dooropen?))))

;; The pure GET status (no PUT)
(define-handler /v1/sm/doorbell-out/status
  (lambda () (status?)))

;; PUT definitions
(define-handler /v1/sm/doorbell-out/lock
  (lambda ()
    (if (current-json)
      (phy-unlock! (alist-ref 'unlock (current-json))))
    (status?)))

(define-handler /v1/sm/doorbell-out/voice
  (lambda ()
  `((acvtive . #f))))

)

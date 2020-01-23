;; Server for in-door part of the doorbell.
(module rest-doorbell-in ()

(import chicken scheme data-structures srfi-1 intarweb spiffy
        srfi-69 srfi-18 data-structures clojurian-syntax)

;; local imports
(use restlib store sm-config gpio looper linphone)

;; Initialize linphone
(lph-create-caller)

(define (fid uid cap)
  (string-hash (conc uid cap)))


(define doorbell-out-store (make-store (string->symbol
                             (conc "doorbell-in" "-"
                               (rest-server-port)))))

;; Parts implemented as physical connection via GPIO
(define phy-unlock-button?  (invert(make-gpio-input 10)))
(define phy-connect-button? (invert(make-gpio-input 12)))

;; Parts implemented as SW modules
(define connect-button-prev 1)
(define connect-state 0)
(define (connect?)
  connect-state)

(define (connect-button-body)
  (if (and (= 0 connect-button-prev) (= 1 (phy-connect-button?)))
    (set! connect-state (bin-toggle connect-state)))
  (set! connect-button-prev (phy-connect-button?)))

(define connect-button-thread
  (thread-start!
    (->>
      connect-button-body
      (loop/interval 0.5)
      (loop)
      ((flip make-thread) "Doorbell-thread"))))

(define (connect-button?)
  connect-button-prev)

;; Common return status definition
(define (status?)
  (append
    `((fid . ,(fid (uid) "doorbell-in"))
      (unlockButton  . ,(phy-unlock-button?)))
      (lph-status)))

;; The pure GET status (no PUT)
(define-handler /v1/sm/doorbell-in/status
  (lambda () (status?)))

;; PUT definitions
(define-handler /v1/sm/doorbell-in/connect
  (lambda ()
    (if (current-json)
      (begin
        (let ((val (alist-ref 'connect (current-json))))
          (if val (lph-call val)))
        (let ((val (alist-ref 'disconnect (current-json))))
          (if val (lph-terminate)))))
    (status?)))
)

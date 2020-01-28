;; Server for out-door part of the doorbell.
(module rest-doorbell-out ()

(import chicken scheme data-structures srfi-1 intarweb spiffy
        srfi-69 srfi-18 data-structures clojurian-syntax)

;; local imports
(use restlib store sm-config gpio looper linphone)

;; Initialize linphone
(lph-create-answerer)

(define (fid uid cap)
  (string-hash (conc uid cap)))


(define doorbell-out-store (make-store (string->symbol
                             (conc "doorbell-out" "-"
                               (rest-server-port)))))


;; Parts implemented as physical connection via GPIO
(define phy-doorbell? (invert(make-gpio-input   3)))
(define phy-unlock?          (make-gpio-input   11))
(define phy-unlock!          (make-gpio-output  7))
(define phy-dooropen?        (make-gpio-input   5))

(define lock-time 0)

(define (unlock! val)
(if (= 0 val)
  (let ((off-age (- (time->seconds (current-time)) lock-time )))
    (print "off-age=" off-age)
    (if (< 8 off-age)
      (phy-unlock! 0)))
  (begin
   (set! lock-time (time->seconds (current-time)))
   (phy-unlock! 1))))

;; Parts implemented as SW modules
(define doorbell-time #f)

(define (doorbell-body)
  (if (not (= 0 (phy-doorbell?)))
    (set! doorbell-time (time->seconds (current-time)))))

(define doorbell-thread
  (thread-start!
    (->>
      doorbell-body
      (loop/interval 0.5)
      (loop)
      ((flip make-thread) "Doorbell-thread"))))

(define (doorbell-age)
  (if doorbell-time
    (round(max 0 (- (time->seconds(current-time)) doorbell-time)))
    -1 ))

;; Common return status definition
(define (status?)
  (append `((fid . ,(fid (uid) "doorbell-out"))
    (doorbell . ,(phy-doorbell?))
    (doorbellAge . ,(doorbell-age))
    (unlock . ,(phy-unlock?))
    (dooropen . ,(phy-dooropen?)))
    (lph-status)))

;; The pure GET status (no PUT)
(define-handler /v1/sm/doorbell-out/status
  (lambda () (status?)))

;; PUT definitions
(define-handler /v1/sm/doorbell-out/doorbell-age
  (lambda ()
    (if (current-json)
      (set! doorbell-time #f))
    (status?)))

(define-handler /v1/sm/doorbell-out/lock
  (lambda ()
    (if (current-json)
      (unlock! (alist-ref 'unlock (current-json))))
    (status?)))

(define-handler /v1/sm/doorbell-out/connect
  (lambda ()
    (if (current-json)
      (let ((val (alist-ref 'disconnect (current-json))))
        (if val (lph-terminate))))
    (status?)))

)

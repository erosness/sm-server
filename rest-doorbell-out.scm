;; Server for out-door part of the doorbell.
(module rest-doorbell-out ()

(import chicken scheme data-structures srfi-1 intarweb spiffy
        srfi-69 srfi-18 data-structures clojurian-syntax)

;; local imports
(use restlib store sm-config gpio looper)

(define (fid uid cap)
  (string-hash (conc uid cap)))


(define doorbell-out-store (make-store (string->symbol
                             (conc "doorbell-out" "-"
                               (rest-server-port)))))


;; Parts implemented as physical connection via GPIO
(define phy-doorbell? (invert(make-gpio-input   3)))
(define phy-unlock?          (make-gpio-input   8))
(define phy-unlock!          (make-gpio-output  7))
(define phy-dooropen?        (make-gpio-input   5))

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
  `((fid . ,(fid (uid) "doorbell-out"))
    (doorbell . ,(phy-doorbell?))
    (doorbellAge . ,(doorbell-age))
    (unlock . ,(phy-unlock?))
    (dooropen . ,(phy-dooropen?))))

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
      (phy-unlock! (alist-ref 'unlock (current-json))))
    (status?)))

(define-handler /v1/sm/doorbell-out/voice
  (lambda ()
  `((acvtive . #f))))

)
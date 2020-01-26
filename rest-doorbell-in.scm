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
(define phy-unlock-button?  (invert(make-gpio-input 3)))
(define phy-connect-button? (invert(make-gpio-input 5)))

(define door-red (make-gpio-output 10))
(define door-green (make-gpio-output 8))
(define voice-red (make-gpio-output 13))
(define voice-green (make-gpio-output 12))

;; Parts implemented as SW modules
(define connect-button-prev 1)
(define connect-state 0)
(define (connect?)
  connect-state)

(define (door-led-work color)
  (case color
    ((red) (door-red 1)(door-green 0) color)
    ((green) (door-red 0)(door-green 1) color)
    ((amber) (door-red 1)(door-green 1) color)
    ((off) (door-red 0)(door-green 0) color)
    ( else  (door-red 0)(door-green 0) 'off)))

(define door-indicator
  (make-parameter 'off door-led-work))

  (define (voice-led-work color)
    (case color
      ((red) (voice-red 1)(voice-green 0) color)
      ((green) (voice-red 0)(voice-green 1) color)
      ((amber) (voice-red 1)(voice-green 1) color)
      ((off) (voice-red 0)(voice-green 0) color)
      ( else  (voice-red 0)(voice-green 0) 'off)))

  (define voice-indicator
    (make-parameter 'off voice-led-work))


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
      (unlockButton  . ,(phy-unlock-button?))
      (callButton . ,(connect-button?))
      (doorIndicator . ,(door-indicator))
      (voiceIndicator . ,(voice-indicator)))
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

(define-handler /v1/sm/doorbell-in/door-indicator
  (lambda ()
    (if (current-json)
      (let ((val (alist-ref 'color (current-json))))
        (door-indicator val)))
    (status?)))

(define-handler /v1/sm/doorbell-in/voice-indicator
  (lambda ()
    (if (current-json)
      (let ((val (alist-ref 'color (current-json))))
        (voice-indicator val)))
    (status?)))
    
)

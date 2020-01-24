;; Server for 8x8 matrix display on the Raspberry Sense HAT.
(module rest-display ()

(import chicken scheme data-structures srfi-1 intarweb spiffy
        srfi-69 srfi-18 data-structures clojurian-syntax)

;; local imports
(use restlib store sm-config gpio looper led-matrix)

(include "led-images/led-image-all.scm")

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

;; Common return status definition
(define (status?)
  (append
    `((fid . ,(fid (uid) "display8x8"))
      (image . ,(symbol->string display-active)))))

;; The pure GET status (no PUT)
(define-handler /v1/sm/display8x8/status
  (lambda () (status?)))

;; PUT definitions

(define display-active 'black)

(define-handler /v1/sm/display8x8/image
  (lambda ()
    (if (current-json)
      (and-let*
        ((img (string->symbol (alist-ref 'image (current-json)))))
          (let*
            ((%time (alist-ref 'time (current-json)))
            (time (if %time %time 0.1))
            (%rep (alist-ref 'repeat (current-json)))
            (rep (if %rep (equal? "yes" %rep) #f)))
            (set! display-active img)
            (case img
              ((ring)  (animate-thread led-image-bell time rep))
              ((didring1)  (animate-thread led-image-didring-1 time rep))
              ((didring2)  (animate-thread led-image-didring-2 time rep))
              ((didring3)  (animate-thread led-image-didring-3 time rep))
              ((didring4)  (animate-thread led-image-didring-4 time rep))
              ((black) (animate-thread led-image-black 0.1 #f))
              ((key) (animate-thread led-image-key 0.1 #f))
              ( else (animate-thread led-image-black 0.1 #f))))))
    (status?)))

)

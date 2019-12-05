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

(define doorbell? (make-gpio-input 12))
(define unlocked? (make-gpio-input 16))
(define unlock!   (make-gpio-output 16))
(define dooropen? (make-gpio-input 18))

(define default-settings
  `((has-lock  . #t)
    (has-video . #f)))

(define-handler /v1/sm/doorbell-out/settings
  (lambda ()
    (if (eq? 'DELETE (request-method (current-request)))
        (current-json default-settings))
    (if (current-json)
        ;; TODO: maybe validate that incoming json has field 'icon'
        ;; with an integer value
        (begin (doorbell-out-store (current-json))
               '((status . "ok")))
        (if (doorbell-out-store)
          (doorbell-out-store)
		      default-settings))))

(define-handler /v1/sm/doorbell-out/status
  (lambda ()
    `((fid . ,(fid (uid) "doorlock"))
      (doorbell . ,(doorbell?))
      (unlock . ,(unlocked?))
      (dooropen . ,(dooropen?)))))


(define-handler /v1/sm/doorbell-out/bell
  (lambda ()
    `((active . ,(doorbell?)))))

(define-handler /v1/sm/doorbell-out/lock
  (lambda ()
    (if (current-json)
      (unlock! (alist-ref 'unlock (current-json))))
    `((unlock . ,(unlocked?)))))

(define-handler /v1/sm/doorbell-out/door
  (lambda ()
  `((open . ,(dooropen?)))))

(define-handler /v1/sm/doorbell-out/voice
  (lambda ()
  `((acvtive . #f))))

)

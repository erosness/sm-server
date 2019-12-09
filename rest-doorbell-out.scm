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

(define phy-doorbell? (make-gpio-input 12))
(define phy-unlock?   (make-gpio-input 16))
(define phy-unlock!   (make-gpio-output 16))
(define phy-dooropen? (make-gpio-input 18))

(define default-settings
  `((has-lock  . #t)
    (has-video . #f)))

(define (status?)
  `((doorbell . ,(phy-doorbell?))
    (unlock . ,(phy-unlock?))
    (dooropen . ,(phy-dooropen?))))

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
  (lambda () (status?)))



(define-handler /v1/sm/doorbell-out/lock
  (lambda ()
    (if (current-json)
      (phy-unlock! (alist-ref 'unlock (current-json))))
    (status?)))


(define-handler /v1/sm/doorbell-out/voice
  (lambda ()
  `((acvtive . #f))))

)

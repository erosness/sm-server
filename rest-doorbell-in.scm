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

(define answer? (make-gpio-input 22))
(define answer! (make-gpio-output 22))
(define unlock? (make-gpio-input 24))
(define unlock! (make-gpio-output 24))

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

(define-handler /v1/sm/doorbell-in/status
  (lambda ()
    `((fid . ,(fid (uid) "doorbell-in"))
      (doorbell . ,(answer?))
      (unlock . ,(unlock?)))))


(define-handler /v1/sm/doorbell-in/bell
  (lambda ()
    `((active . ,(answer?)))))

(define-handler /v1/sm/doorbell-in/lock
  (lambda ()
    (if (current-json)
      (answer! (alist-ref 'answer (current-json))))
    `((answer . ,(answer?)))))


)

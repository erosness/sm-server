;; Server for out-door part of the doorbell.
(module rest-doorbell-out ()

(import chicken scheme data-structures srfi-1)

(use intarweb spiffy
     medea matchable irregex ports clojurian-syntax restlib srfi-18 extras
     posix srfi-1 srfi-13
     (only posix with-input-from-pipe))

;; local imports
(import restlib store sm-config)

(define doorbell-out-store (make-store (string->symbol
                                   (conc "doorbell-out" "-"
                                         (rest-server-port)))))

(define default-settings
  `((name . "noname")
  (uid  . ,(uid))))

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

(define-handler /v1/sm/doorbell-out/bell
  (lambda ()
    `((ringing . #f))))

(define-handler /v1/sm/doorbell-out/lock
  (lambda ()
  `((unlock . #f))))

(define-handler /v1/sm/doorbell-out/voice
  (lambda ()
  `((avtive . #f))))

)

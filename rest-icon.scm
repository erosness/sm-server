(module rest-icon ()

(import chicken scheme)

;; local imports
(import restlib store)

(define speaker-store (make-store 'speaker-icon))

(define-handler /v1/player/icon
  (lambda ()
    (if (current-json)
        ;; TODO: maybe validate that incoming json has field 'icon'
        ;; with an integer value
        (begin (speaker-store (current-json))
               '((status . "ok")))
        (or (speaker-store)
            `((icon . 0)))))))

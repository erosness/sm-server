(module rest-icon ()

(import chicken scheme data-structures)

;; local imports
(import restlib store)

(define speaker-store (make-store (string->symbol
                                   (conc "speaker-icon" "-"
                                         (rest-server-port)))))

(define-handler /v1/player/icon
  (lambda ()
    (if (current-json)
        ;; TODO: maybe validate that incoming json has field 'icon'
        ;; with an integer value
        (begin (speaker-store (current-json))
               '((status . "ok")))
        (or (speaker-store)
            `((icon . 0)
              (name . "")))))))

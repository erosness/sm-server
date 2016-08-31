(module rest-icon ()

(import chicken scheme data-structures)

(use intarweb spiffy
     (only posix with-input-from-pipe))

;; local imports
(import restlib store)

(define speaker-store (make-store (string->symbol
                                   (conc "speaker-icon" "-"
                                         (rest-server-port)))))

(define empty-value `((icon . 0)
                      (name . "")))

(define-handler /v1/player/icon
  (lambda ()
    (if (eq? 'DELETE (request-method (current-request)))
        (current-json empty-value))

    (if (current-json)
        ;; TODO: maybe validate that incoming json has field 'icon'
        ;; with an integer value
        (begin (speaker-store (current-json))
               (with-input-from-pipe (conc "spotifyctl 7879 label "
                                           (alist-ref 'name (current-json)))
                                     void)
               '((status . "ok")))
        (or (speaker-store)
            empty-value)))))

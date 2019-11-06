;; Server for out-door part of the doorbell.
(module rest-doorbell-out ()

(import chicken scheme data-structures srfi-1)

(use intarweb spiffy
     medea matchable irregex ports clojurian-syntax restlib srfi-18 extras
     posix srfi-1 srfi-13
     (only posix with-input-from-pipe))

;; local imports
(import restlib store sm-config)

(define speaker-store (make-store (string->symbol
                                   (conc "speaker-icon" "-"
                                         (rest-server-port)))))

(define empty-value
  `((name . "noname")
  (uid  . ,uid)))

(define-handler /v1/sm/doorbell-out
  (lambda ()
    (if (eq? 'DELETE (request-method (current-request)))
        (current-json empty-value))

    (if (current-json)
        ;; TODO: maybe validate that incoming json has field 'icon'
        ;; with an integer value
        (begin (speaker-store (current-json))
               '((status . "ok")))
        (if (speaker-store)
            (let* ((%alist-raw ( alist-delete 'ip_audio (alist-delete 'uid_leader (alist-delete 'uid (speaker-store)))))
		   (%alist-with-name (if (alist-ref 'name %alist-raw)
                                         %alist-raw
                                         (alist-cons 'name (alist-ref 'name empty-value) %alist-raw)))
		   (%alist-name-icon (if (and (alist-ref 'icon %alist-with-name)
					      (< 0 (alist-ref 'icon %alist-with-name)))
					 %alist-with-name
					 (alist-cons  'icon (alist-ref 'icon empty-value) (alist-delete 'icon %alist-with-name)))))
				      (alist-cons 'uid uid %alist-name-icon))
            empty-value))))
)

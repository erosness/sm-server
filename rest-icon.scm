(module rest-icon ()

(import chicken scheme data-structures srfi-1)

(use test restlib clojurian-syntax ports
     srfi-18 extras posix srfi-1 srfi-13
     medea matchable irregex matchable medea matchable irregex  intarweb spiffy
     (only posix with-input-from-pipe))

;; local imports
(import restlib store)

(define uid_leader 
  (irregex-match-substring
   (irregex-search
    '(: "inet " (=> ip (or "192" "10") ;; assuming your LAN has this ip
                    (= 1 "." (** 1 3 numeric))
                    (= 1 ".42")
                    (= 1 "." (** 1 3 numeric))
                                    ) )
    (with-input-from-pipe "ip a|grep inet" read-string))
   'ip))

(define speaker-store (make-store (string->symbol
                                   (conc "speaker-icon" "-"
                                         (rest-server-port)))))

(define empty-value `((icon . 0)
                      (name . "")
                      (uid_leader  . ,uid_leader)
                      (type . "M")) )

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
        (if (speaker-store)
            (alist-cons 'uid_leader uid_leader (alist-delete 'uid_leader (speaker-store)) )
            empty-value)))))

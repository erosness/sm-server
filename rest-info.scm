
(module rest-info ()

(import chicken scheme data-structures srfi-1)

(use intarweb spiffy
     medea matchable irregex ports clojurian-syntax restlib srfi-18 extras
     posix srfi-1 srfi-13
     (only posix with-input-from-pipe))

;; local imports
(import restlib store)

(cond-expand
  (arm
    (define mac (with-input-from-file "/sys/class/net/eth0/address" read-string)))
  (else
    (define mac (with-input-from-file "/sys/class/net/enp0s31f6/address" read-string))))

(define find_ip_leader
  (irregex-match-substring
   (irregex-search
    '(: "inet " (=> ip (or"192" "10")
		    (= 1 "." (** 1 3 numeric))
		    (= 1 ".42")
		    (= 1 "." (** 1 3 numeric))
		    ))
    (conc (with-input-from-pipe "ip a|grep inet" read-string) " inet 192.168.42.1"))
   'ip))

(define speaker-store (make-store (string->symbol
                                   (conc "speaker-icon" "-"
                                         (rest-server-port)))))

(define empty-value
  `((name . "noname")
  (uid  . ,mac)))

(define-handler /v1/sm/info
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
	      (alist-cons 'ip_audio find_ip_leader
			  (alist-cons 'uid_leader find_ip_leader
				      (alist-cons 'uid mac %alist-name-icon))))
            empty-value))))
)

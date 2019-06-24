
(module rest-icon ()

(import chicken scheme data-structures srfi-1)

(use intarweb spiffy
     medea matchable irregex ports clojurian-syntax restlib srfi-18 extras
     posix srfi-1 srfi-13
     (only posix with-input-from-pipe))

;; local imports
(import incubator restlib store)

(define mac
  (conc
    (read (open-input-file "/sys/fsl_otp/HW_OCOTP_MAC1"))
    ":"
    (read (open-input-file "/sys/fsl_otp/HW_OCOTP_MAC0"))))

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

(cond-expand
(android
  (define empty-value
    `((uid_leader . "192.168.42.1")
    (icon . 8)
    (name . "Maestro")
    (uid  . ,mac)
    (type . "M"))))
(else
  (define empty-value
     `((uid_leader . ,find_ip_leader )
     (icon . 0)
     (name . "")
     (uid  . ,mac)
     (type . "S")))))

(define empty-value
   `((icon . 1)
   (name . "Record Player")
   (turi  . ,(turi) )))

(define-handler /v1/source/icon
  (lambda ()
    (if (eq? 'DELETE (request-method (current-request)))
        (current-json empty-value))

    (if (current-json)
      ;; TODO: maybe validate that incoming json has field 'icon'
      ;; with an integer value
      (begin (source-store (current-json))
             '((status . "ok")))
      (if (source-store)
        (alist-merge empty-value (source-store) `((turi . ,(turi))))
        empty-value))))
)

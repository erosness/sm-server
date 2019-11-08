;; Server for out-door part of the doorbell.
(module rest-doorbell-out ()

(import chicken scheme)

(use medea restlib (only posix with-input-from-pipe))

;; local imports
(import restlib)

(define (published-units)
  (with-input-from-pipe
    "avahi-browse -rtp _sm._tcp | grep = | grep IPv4 | awk -F ';' 'BEGIN {printf \"(\"}{printf \"(\\\x22%s\\\x22 %s)\", $8, $9}END {printf \")\"}'"
     read))

(define (out u)
  `((ip   . ,(car u))
    (port . ,(cadr u))))

(define-handler /v1/sm/zeroconf
  (lambda ()
    `((published-units . ,(list->vector (map out (published-units)))))))

)

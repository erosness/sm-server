
;;; load this file from your repl for quick REPL setup.

(include "cube-server.scm")

(define port 5060)
(server-port port)

(define dns-sd-unregister!/browser (dns-sd-register "repl" port service-type/cube-browser))
(define dns-sd-unregister!/pq      (register-pq-with-icon-store "repl" port))
(define server-thread (start-rest-server!))

(import rest-wimp)

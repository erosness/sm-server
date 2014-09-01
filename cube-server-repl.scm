
;;; load this file from your repl for quick REPL setup.

(include "cube-server.scm")

(define dns-sd-unregister!/browser (dns-sd-register "repl" 5060 service-type/cube-browser))
(define dns-sd-unregister!/pq      (register-pq-with-icon-store "repl" 5060))
(define server-thread (start-rest-server!))

(import rest-wimp)

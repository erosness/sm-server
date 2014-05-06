
(include "cube-server.scm")

(include "job-util.scm") ;; job-auto-respawn

(define dns-sd-unregister! (dns-sd-register (conc hostname "-cube") port))

(if (not (= 1 (length (command-line-arguments))))
    (error "usage: cube-server <port>"))
(start-server port: (string->number (car (command-line-arguments))))



(include "cube-server.scm")

(include "job-util.scm") ;; job-auto-respawn

(define *thread-heartbeat*
  (thread-start! (lambda () (job-auto-respawn (lambda () (start-discovery 5055 360))))))

(if (not (= 1 (length (command-line-arguments))))
    (error "usage: cube-server <port>"))
(start-server port: (string->number (car (command-line-arguments))))


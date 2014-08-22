;; minimum includes for cspeaker to work
(use pefat)
(use spiffy matchable medea restlib)

(include "args.scm")

(with-args
 (list (args:make-option (n) #:none "run non-interactively"))
 (lambda (opts nickname #!optional (port/str "5060"))
   (define port (string->number port/str))
   ;; bind dynamic paramter for everybody to use.
   (rest-server-port port)

   ;; does not contain deps (it's included from cube-server.scm too)
   (include "cspeaker.scm")
   (import store)

   (print "started cspeaker on http://localhost:" port)

   (import rest)
   (define dns-sd-unregister!/pq (register-pq-with-icon-store nickname port))
   (define server-thread (start-rest-server!))

   (if (alist-ref 'n opts)
       (thread-join! server-thread)
       (repl*))))

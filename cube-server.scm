(use spiffy intarweb uri-common
     srfi-69 restlib
     clojurian-syntax)

(use pefat)
(include "debug-utils.scm")

(include "concurrent-utils.scm")
(include "process-cli.scm")

(include "broadcast.scm")
(include "player.scm")
(include "playqueue.scm")

(include "rest.scm")
(include "rest-tone.scm")
(include "rest-notes.scm")
(include "rest-wimp.scm")
(include "rest-dlna.scm")
(include "rest-pq.scm")
(include "rest-player.scm")
(include "rest-usb.scm")

;; cube-server discovery (not ssdp!)
(include "dns-sd.scm")

(import rest)

(define handler (->> (lambda () (json-handler))
                     (wrap-json)
                     (wrap-errors)))

(vhost-map `((".*" . ,(lambda (continue) (handler)))))

;; for your repl pleasure:
;; (define thread (thread-start! (lambda () (start-server port: 5055))))
;; (define thread-sf (thread-start! (lambda () (start-static-file-server))))
;; (pp (hash-table->alist *uris*))


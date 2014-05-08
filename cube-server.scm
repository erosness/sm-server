(use spiffy intarweb uri-common
     srfi-69 restlib
     clojurian-syntax)

;; this guys takes in a lot of our dependencies as well
(include "cspeaker.scm")

(include "rest-tone.scm")
(include "rest-notes.scm")
(include "rest-wimp.scm")
(include "rest-dlna.scm")
(include "rest-player.scm")
(include "rest-usb.scm")

(include "dummy-browser.scm")


;; for your repl pleasure:
;; (define thread (thread-start! (lambda () (start-server port: 5055))))
;; (define thread-sf (thread-start! (lambda () (start-static-file-server))))
;; (pp (hash-table->alist *uris*))


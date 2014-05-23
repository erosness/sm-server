(use spiffy intarweb uri-common
     srfi-69 restlib
     clojurian-syntax)

;; this guys takes in a lot of our dependencies as well
(include "cspeaker.scm")

(include "turi.scm")

(include "rest-tone.scm")
(include "rest-notes.scm")
(include "rest-wimp.scm")
(include "rest-dlna.scm")
(include "rest-usb.scm")

(include "dummy-browser.scm")


;; for your repl pleasure:
;; (define dns-sd-unregister!/browser (dns-sd-register "repl" 5060 service-type/cube-browser))
;; (define dns-sd-unregister!/pq      (dns-sd-register "repl" 5060 service-type/cube-pq))
;; (define server-thread (start-rest-server! 5060))

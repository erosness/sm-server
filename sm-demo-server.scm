(use pefat)
(require-extension utf8)
(require-extension utf8-srfi-13)
(require-extension utf8-srfi-14)
;; this guys takes in a lot of our dependencies as well
(include "cspeaker.scm")

(include "turi.scm")


(include "rest-tone.scm")
(include "rest-alsa-capture.scm")
(include "rest-record-player.scm")
;; (include "rest-group.scm")

(use pefat)
(require-extension utf8)
(require-extension utf8-srfi-13)
(require-extension utf8-srfi-14)
;; this guys takes in a lot of our dependencies as well
(include "cspeaker.scm")

(include "turi.scm")

(include "rest-tone.scm")
(include "rest-notes.scm")
(include "rest-wimp.scm")
(include "rest-dlna.scm")
(include "rest-usb.scm")
(include "rest-tunein.scm")
(include "rest-alsa-capture.scm")
;; (include "rest-group.scm")

(include "discover.scm")

(cond-expand
 ((not arm)
  (include "rest-dummy-dab.scm")
  (include "rest-dummy-fm.scm")
  )
 (else
  (include "rest-bluetooth.scm")
  (include "rest-dab.scm")
  (include "rest-fm.scm")
  ))



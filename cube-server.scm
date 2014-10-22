(use pefat)
;; this guys takes in a lot of our dependencies as well
(include "cspeaker.scm")

(include "turi.scm")

(include "rest-tone.scm")
(include "rest-notes.scm")
(include "rest-wimp.scm")
(include "rest-dlna.scm")
(include "rest-usb.scm")
(include "rest-tunein.scm")

(cond-expand
 ((not arm)
  (include "rest-dummy-dab.scm"))
 (else
  (include "rest-dab.scm")
  (include "rest-fm.scm")
  ))



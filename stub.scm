(import scheme chicken)

(use linphone)

(define stub-lc (lph-create-caller))

(define sts
  (concatenate (`(( per . 1)(kaare . knut)) (lph-status))))

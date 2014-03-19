(use test fmt dlna)

;; ==================== formatting ====================
(define (fmt-services services)
  (tabular "  "
           (fmt-join dsp (map car services) nl)
           " "
           (fmt-join dsp (map cdr services) nl)))

(define (fmt-device lst)
  (let ((url (car lst)))
    (cat nl "========== " url nl
         (fmt-services (cdr lst)))))

;; print discovered devices nicely
(define (devices) (fmt #t (fmt-join fmt-device (*devices*))))

(print "
==================== simple dlna client ====================
commands:
search ;; multicast UPnP/SSDP search and print responses for 60sec
ls <q> ;; query all (*devices*) found for <q> (using UPnP builtin search func)

;; searching ...
")

(search)
(repl)
;; (ls "jackson")
;; (devices)

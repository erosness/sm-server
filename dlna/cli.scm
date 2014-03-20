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

(define *devices* (ssdp-search))
;; print discovered devices nicely
(define (status) (fmt #t (fmt-join fmt-device (*devices*))))

(define (search q) (dlna-search/track (*devices*) q))

(print "
==================== simple dlna client ====================
commands:
(status)            ;; print discoveried services thus far
(search <q>) ;; search for <q> on all devices, using UPnP builtin search func

;; searching ...
")

(repl)
;; (ls "jackson")
;; (devices)

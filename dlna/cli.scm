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
(define (status) (fmt #t (fmt-join fmt-device (*devices*))))

(define search dlna-search)

(print "
==================== simple dlna client ====================
commands:
search <q>        ;; search for <q> on all devices, using UPnP builtin search func
status            ;; print discoveried services thus far
start-ssdp-search ;; multicast UPnP/SSDP search and print responses for 60sec

;; searching ...
")

(start-ssdp-search)
(repl)
;; (ls "jackson")
;; (devices)

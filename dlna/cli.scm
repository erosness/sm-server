(use test fmt dlna)

;; ==================== formatting ====================
(define (fmt-services services)
  (tabular "  "
           (fmt-join dsp (map car services) nl)
           " "
           (fmt-join dsp (map cdr services) nl)))

(define (fmt-device dev)
  (let ((url (car dev)))
    (cat nl "========== " url nl
         (fmt-services (ssdp-device-services dev)))))

(define *devices* (ssdp-search))
;; print discovered devices nicely
(define (status) (fmt #t (fmt-join fmt-device (*devices*))))

(define (search q)
  (append-map (lambda (service)
                (handle-exceptions e (begin (pp (condition->list e)) '())
                                   (dlna-search/track service q)))
              (append-map ssdp-device-content-directories (*devices*))))

(print "
==================== simple dlna client ====================
commands:
(status)            ;; print discoveried services thus far
(search <q>) ;; search for <q> on all devices, using UPnP builtin search func

;; searching ...
")

(repl)
;; (search "jackson")
;; (pp (*devices*))

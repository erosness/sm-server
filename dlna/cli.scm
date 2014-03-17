(use test fmt)

(include "ssdp.scm")
(include "root.scm")
(include "didl.scm")


;; search for ssdp servers for 60 seconds, and print their locations
(define *devices* (lambda () '()))

;; query a location (rootdesc url) for content-directory:1
;; control-urls
(define (query-cdurls loc)
  (let* ((qurls (query-control-urls loc))
         (cdurls (map cdr (filter content-directory:1? qurls))))
    (print "services: ")
    (pp qurls)
    cdurls))

(define (search)
  (set! *devices*
        (ssdp-search* 60
                      (lambda (packet lst)
                        (let ((l (packet-location packet)))
                          ;; avoid duplicates
                          (if (member l (map car lst))
                              lst
                              (begin (print "discovered: " l)
                                     (cons (cons l (query-control-urls l)) lst)))))
                      '())))

(define (ls q)
  (map
   (lambda (services) ;; <-- ((servicetype . url) (servicetype . url) ...)
     (let* ()
       (map (lambda (service)
              (let ((cdurl (cdr service)))
                (print ";; ========== querying " cdurl)
                (->> (search-query cdurl (conc "dc:title contains \"" q "\""))
                     (didl->talist)
                     (pp))))
            ;; we can search on content-directory services only
            (filter content-directory:1? services))))
   (map cdr (*devices*))))

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

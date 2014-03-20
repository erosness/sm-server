
(use clojurian-syntax)

(include "ssdp.scm")
(include "root.scm")
(include "didl.scm")

;; TODO: remove this global variable and parameterize something
;; somewhere.
(define *devices* (lambda () '()))

;; search for ssdp servers for 60 seconds, and print their locations
(define (start-ssdp-search)
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

;; search all discovered devices for string 'q'
(define (dlna-search q)
  (append-map
   (lambda (services) ;; <-- ((servicetype . url) (servicetype . url) ...)
     (let* ()
       (append-map (lambda (service)
                     (let ((cdurl (cdr service)))
                       (->> (search-query cdurl (conc "dc:title contains \"" q "\""))
                            (didl->talist))))
                   ;; we can search on content-directory services only
                   (filter content-directory:1? services))))
   (map cdr (*devices*))))



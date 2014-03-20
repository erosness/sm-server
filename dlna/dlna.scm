(use clojurian-syntax srfi-1)

(include "ssdp.scm")
(include "root.scm")
(include "didl.scm")

;; fold procedure for ssdp-search, creating a list of
;; (rooturl (type . ctr-url) ...)
;; this isn't in ssdp.scm because we need root's query-control-urls
(define (%ssdp-search-fold packet lst)
  (let ((l (packet-location packet)))
    ;; avoid duplicates
    (if (member l (map car lst))
        lst
        (cons (cons l (query-control-urls l)) lst))))

;; search for surrounding UPnP services. returns a procedure which
;; returns devices discovered this far:
;;
;; ( (rooturl (service-type . ctr-url) ...) ...)
(define (ssdp-search #!optional (timeout/sec 60))
  (ssdp-search* timeout/sec
                %ssdp-search-fold
                '()))

;; given a list of UPnP devices, returns a list of control urls for
;; all ContentDirectory:1's.
(define (content-directories devices)
  (append-map
   (lambda (services) ;; <-- ((servicetype . url) (servicetype . url) ...)
     (filter-map ContentDirectory:1 services))
   (map cdr devices))) ;; <-- remove rootdesc url


;; search all devices for search-criteria (UPnP spec)
(define (dlna-search devices search-criteria)
  (append-map
   (lambda (cdurl)
     (->> (search-query cdurl search-criteria)
          (didl->talist)))
   (content-directories devices)))

(define ((%search proc) devices q)
  (dlna-search devices (proc q)))

(define dlna-search/artist (%search (cut conc "upnp:artist contains \"" <> "\"")))
(define dlna-search/album  (%search (cut conc "upnp:album contains \""  <> "\"")))
(define dlna-search/track  (%search (cut conc "dc:title contains \""    <> "\"")))

(include "dlna.test.scm")

;; (define *devices* (ssdp-search))
;; (pp (*devices*))
;; (pp (dlna-search/track (*devices*) "michael"))

(use clojurian-syntax srfi-1)

(include "ssdp.scm")
(include "root.scm")
(include "didl.scm")


;; nice convenience wrappers around root.scm's API
(define (make-ssdp-device root-location doc) (cons root-location doc))
(define (ssdp-device-root-location device)   (car device))
(define (ssdp-device-doc device)             (cdr device))
(define (ssdp-device-friendly-name device)   (friendly-name (ssdp-device-doc device)))
(define (ssdp-device-model-name device)      (model-name (ssdp-device-doc device)))


;; xml urls may be relative to:
;; - an root-url specified in the XML document itself
;; - the root-descritor host from which the XML document was retrieved
(define (ssdp-device-services device)
  (let ((doc (ssdp-device-doc device)))
    (service-alist doc
                   (or (base-url doc) ;; take base-url from doc if present
                       (ssdp-device-root-location device))))) ;; otherwise use request url

;; like ssdp-device-services, but filter out just the
;; ContentDirectory:1 services that can be used for searching and
;; browsing of files.
(define (ssdp-device-content-directories device)
  (filter-map ContentDirectory:1 (ssdp-device-services device)))

;; fold procedure for ssdp-search, creating a list of devices. each
;; device is: (rooturl . doc)
;; this isn't in ssdp.scm because we need root's query-control-urls
;; TODO: this wraps a nice debug-printer. this debug-printer can be macroified!
(define (%ssdp-search-fold packet address lst)
  (handle-exceptions e
    (begin
      (write `(error ssdp-search
                     ,packet
                     ,address
                     ,(condition->list e)))
      (newline)
      lst)
    (let ((l (packet-location packet)))
      ;; avoid duplicates
      (if l ;; <--packet location may fail
          (if (member l (map ssdp-device-root-location lst))
              lst
              (let ((doc (rootdesc-query l)))
                (if doc (cons (make-ssdp-device l doc) lst)
                    ;; we can't connect to or parse the root doc:
                    (begin (write `(ssdp-search (error doc root)
                                                ,l ,packet))
                           (newline)
                           lst))))
          lst))))

;; search for surrounding UPnP services. returns a procedure which
;; returns devices discovered this far
(define (ssdp-search #!optional (timeout/sec 60))
  (ssdp-search* timeout/sec
                %ssdp-search-fold
                '()))

;; search all devices for search-criteria (UPnP spec)
(define (dlna-search service-url search-criteria)
  (->> (search-query service-url search-criteria)
       (didl->talist)))

(define ((%search proc) service-url q)
  (dlna-search service-url (proc q)))

(define dlna-search/artist (%search (cut conc "upnp:artist contains \"" <> "\"")))
(define dlna-search/album  (%search (cut conc "upnp:album contains \""  <> "\"")))
(define dlna-search/track  (%search (cut conc "dc:title contains \""    <> "\"")))

(include "dlna.test.scm")

;; (define *devices* (ssdp-search))
;; (pp (*devices*))
;; (pp (dlna-search/track "http://localhost:8000/ctl/ContentDir" "michael"))

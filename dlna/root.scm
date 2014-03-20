(use ssax sxpath test clojurian-syntax)

(include "sxml-common.scm")

(define (%device element #!optional (end "/text()"))
  (conc "dev:root/dev:device/" element end))
(define device-type   (sxpath/car (%device "dev:deviceType") ns))
(define friendly-name (sxpath/car (%device "dev:friendlyName") ns))
(define services      (sxpath     "*//dev:serviceList/dev:service" ns))
(define service-type  (sxpath/car "dev:serviceType/text()" ns))
(define control-url   (sxpath/car "/dev:controlURL/text()" ns))
;; if #f, use the same hostname as root-description:
(define base-url      (sxpath/car "*//dev:URLBase/text()" ns))

;; returns url of content-directory or #f if service-type isn't a
;; ContentDirectory:1.
(define (ContentDirectory:1 pair)
  (and (pair? pair)
       (eq? (car pair) 'urn:schemas-upnp-org:service:ContentDirectory:1)
       (cdr pair)))

(define (url->base-url base #!optional (path '()))
  (uri->string (update-uri (uri-reference base) path: path)))

(define (media-server? doc)
  (equal? (device-type doc)
          "urn:schemas-upnp-org:device:MediaServer:1"))

;; control-url as an absolute url.
(define (absolute-control-url baseurl sdoc)
  (and-let* ( ;; eg "/ctr/ContentDir or "http://10.0.0.89/ctr"
             (ctr-url (control-url sdoc))
             (ctr-uri (uri-reference ctr-url)))
    (if (absolute-uri? ctr-uri)
        (uri->string ctr-uri)
        (url->base-url baseurl (uri-path ctr-uri)))))


(define (service-alist doc #!optional (baseurl (base-url doc)))
  (map (lambda (s)
         (cons (string->symbol (service-type s))
               (absolute-control-url baseurl s)))
       (services doc)))

;; perform a HTTP request against uri, returning response as sxml
(define (rootdesc-query uri)
  (define (read-sxml) (ssax:xml->sxml (current-input-port) '()))
  (values (with-input-from-request uri #f read-sxml)))


;; query an UPnP server's rootdescriptor for it's ContentDirectory:1
;; control urls. returns #f if none found. returned url is always
;; absolute.
(define (query-control-urls rootdesc-url)
  (let ((doc (rootdesc-query rootdesc-url)))
    (service-alist doc (or (base-url doc) ;; take base-url from doc if present
                           rootdesc-url   ;; otherwise use request url
                           ))))


(include "root.test.scm")

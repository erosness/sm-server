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

(define (string-maybe-drop-slash/right str)
  (if (string-suffix? "/" str)
      (string-drop-right str 1)
      str))

(test-group
 "string-maybe-drop-slash"
 (test "a" (string-maybe-drop-slash/right "a"))
 (test "a" (string-maybe-drop-slash/right "a/")))

(define (url->base-url base ctruri)
  (conc (string-maybe-drop-slash/right
         (uri->string (update-uri (uri-reference base)
                                  path: '()
                                  query: #f)))
        ctruri))

(define (media-server? doc)
  (equal? (device-type doc)
          "urn:schemas-upnp-org:device:MediaServer:1"))

;; control-url as an absolute url.
(define (absolute-control-url baseurl sdoc)
  (and-let* ( ;; eg "/ctr/ContentDir or "http://10.0.0.89/ctr"
             (ctr-url (control-url sdoc)))
    (if (absolute-uri? (uri-reference ctr-url))
        ctr-url
        (url->base-url baseurl ctr-url))))


(define (service-alist doc #!optional (baseurl (base-url doc)))
  (filter-map
   (lambda (s)
     (and-let* ((st (service-type s)))
       (cons (string->symbol st)
             (absolute-control-url baseurl s))))
   (services doc)))

(use http-client uri-common intarweb)
;; stolen from closing-http-client.scm. how should this be properly shared?
(define (with-input-from-request* req writer reader)
  (let ((req (cond ((request? req) req)
                   ((uri? req) (make-request uri: req))
                   ((string? req) (make-request uri: (uri-reference req))))))
    (with-input-from-request
     (update-request
      req
      headers: (replace-header-contents 'connection
                                        '(#(close ()))
                                        (request-headers req)))
     writer reader)))


;; perform a HTTP request against uri, returning response as sxml
(define (rootdesc-query uri)
  (define (read-sxml) (ssax:xml->sxml (current-input-port) '()))
  (values (condition-case
           (with-input-from-request* uri #f read-sxml)
           ((exn http client-error) #f)
           ;; in case of error, just print it for now and return #f
           (e ()
              (pp `(dlna warning ,uri ,(condition->list e)))
              #f))))

;; query an UPnP server's rootdescriptor for it's control urls.
;; returns #f if none found. returned url is always absolute (that's
;; why we need the original url)
(define (query-control-urls rootdesc-url)
  (and-let* ((doc (rootdesc-query rootdesc-url)))
    (handle-exceptions e
      (begin (pp
              `(error query-control-urls
                      ,doc
                      ,(condition->list e))))
      (service-alist doc (or (base-url doc) ;; take base-url from doc if present
                             rootdesc-url ;; otherwise use request url
                             )))))


(include "root.test.scm")

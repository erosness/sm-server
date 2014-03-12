
;; we actually need this silly little thing because car-sxpath doesn't
;; accept namespaces
(define (sxpath/car search ns)
  (let ((proc (sxpath search ns)))
    (lambda (doc) (car (proc doc)))))

(define ns '((pe   . "http://purl.org/dc/elements/1.1/")
             (u    . "urn:schemas-upnp-org:metadata-1-0/upnp/")
             (d    . "urn:schemas-upnp-org:metadata-1-0/DIDL-Lite/")
             (e    . "http://schemas.xmlsoap.org/soap/envelope/")
             (cd1  . "urn:schemas-upnp-org:service:ContentDirectory:1")
             (dev  . "urn:schemas-upnp-org:device-1-0")))

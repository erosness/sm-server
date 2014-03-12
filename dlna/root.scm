(use ssax sxpath test)

(include "sxml-common.scm")

(define (%device element #!optional (end "/text()"))
  (conc "dev:root/dev:device/" element end))
(define device-type   (sxpath/car (%device "dev:deviceType") ns))
(define friendly-name (sxpath/car (%device "dev:friendlyName") ns))
(define services      (sxpath     (%device "dev:serviceList/dev:service" "") ns))
(define service-type  (sxpath/car "dev:serviceType/text()" ns))
(define control-url   (sxpath/car "/dev:controlURL/text()" ns))

(define (media-server? doc)
  (equal? (device-type doc)
          "urn:schemas-upnp-org:device:MediaServer:1"))

(include "root.test.scm")

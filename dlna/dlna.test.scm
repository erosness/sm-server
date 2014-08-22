(use test)


;; test our simple data-structures and helpers
(test-group
 "ssdp-device and friends"
 (define doc
   '(*TOP* (*PI* xml "version=\"1.0\" encoding=\"UTF-8\" ")
           (urn:schemas-upnp-org:device-1-0:root
            (urn:schemas-upnp-org:device-1-0:device
             (urn:schemas-upnp-org:device-1-0:friendlyName "MPC3001")
             (urn:schemas-upnp-org:device-1-0:modelName "Aficio MP C3001")
             (urn:schemas-upnp-org:device-1-0:serviceList
              (urn:schemas-upnp-org:device-1-0:service
               (urn:schemas-upnp-org:device-1-0:serviceType "st")
               (urn:schemas-upnp-org:device-1-0:serviceId "sid")
               (urn:schemas-upnp-org:device-1-0:controlURL "/ctl/url")))))))

 (define dev (make-ssdp-device "http://root" doc))

 (test "http://root" (ssdp-device-root-location dev))
 (test "MPC3001" (ssdp-device-friendly-name dev))
 (test "Aficio MP C3001" (ssdp-device-model-name dev))
 (test `((st . "http://root/ctl/url")) (ssdp-device-services dev)))


(test
 "%ssdp-search-fold"
 '( ("http://a.com" . (*TOP* (element))) )
 ;; rootdesc-query requires network & server - mock it!
 (fluid-let ((rootdesc-query (lambda (l) '(*TOP* (element)))))
   (%ssdp-search-fold "http/1.1 200 ok\nlocation: http://a.com\n\n" 'addresss '())))

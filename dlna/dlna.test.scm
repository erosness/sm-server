(use test)


(test
 "%ssdp-search-fold"

 '(  ("http://b.com" (service . "ctrurl"))
     ("http://a.com" (service . "ctrurl")))

 ;; query-control-urls requires network & server
 (fluid-let ((query-control-urls (lambda (l) '((service . "ctrurl")))))
   (->> '()
        (%ssdp-search-fold "http/1.1 200 ok\nlocation: http://a.com\n\n" #f)
        (%ssdp-search-fold "http/1.1 200 ok\nlocation: http://b.com\n\n" #f))))


(test
 "content-directories from devices"
 '("cdurl")
 (content-directories
  '( ("http://10.0.0.19:8200/rootDesc.xml" ;; my MiniDLNA with mods
      (urn:schemas-upnp-org:service:ContentDirectory:1 . "cdurl")
      (urn:schemas-upnp-org:service:ConnectionManager:1 . "cmurl")
      (urn:microsoft.com:service:X_MS_MediaReceiverRegistrar:1 . "msurl")))))

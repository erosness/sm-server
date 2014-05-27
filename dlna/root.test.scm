

 ;; to test our understanding of sxpath:
(test "control-url"
      "/wan"
      (control-url '(anything (urn:schemas-upnp-org:device-1-0:controlURL "/wan"))))


(test "content-directory:1?"
      '(#f #f "y")
      (map ContentDirectory:1
           '((a . "n")
             (b . "n")
             (urn:schemas-upnp-org:service:ContentDirectory:1 . "y"))))



;; some servers (like, you guessed it, Microsoft's) don't properly
;; parse encoded urls. so we don't either, and just return what the
;; server originally gave us unmodified.
(test-group
 "url->base-url"

  (test "url->base-url strips double-slashes"
       "http://server.com/a" ;; not //a !
       (url->base-url "http://server.com/ignored" "/a"))

 (test "url->base-url allows crazy uri paths"
       "http://server.com/{foo^ }"
       (url->base-url "http://server.com/path?key=value;don'tinclude-this" "/{foo^ }")))


(test-group
 "extract from services"

 ;; rootDesc.xml from MiniDLNA on Linux
 (define doc
   '(*TOP* (*PI* xml "version=\"1.0\"")
           (urn:schemas-upnp-org:device-1-0:root
            (urn:schemas-upnp-org:device-1-0:specVersion
             (urn:schemas-upnp-org:device-1-0:major "1")
             (urn:schemas-upnp-org:device-1-0:minor "0"))
            (urn:schemas-upnp-org:device-1-0:device
             (urn:schemas-upnp-org:device-1-0:deviceType "urn:schemas-upnp-org:device:MediaServer:1")
             (urn:schemas-upnp-org:device-1-0:friendlyName "Kristian Thinkpad")
             (urn:schemas-upnp-org:device-1-0:manufacturer "Justin Maggard")
             (urn:schemas-upnp-org:device-1-0:manufacturerURL "http://www.netgear.com/")
             (urn:schemas-upnp-org:device-1-0:modelDescription "MiniDLNA on Linux")
             (urn:schemas-upnp-org:device-1-0:modelName "Windows Media Connect compatible (MiniDLNA)")
             (urn:schemas-upnp-org:device-1-0:modelNumber "1")
             (urn:schemas-upnp-org:device-1-0:modelURL "http://www.netgear.com")
             (urn:schemas-upnp-org:device-1-0:serialNumber "12345678")
             (urn:schemas-upnp-org:device-1-0:UDN "uuid:4d696e69-444c-164e-9d41-100ba9e3a0fc")
             (urn:schemas-dlna-org:device-1-0:X_DLNADOC "DMS-1.50")
             (urn:schemas-upnp-org:device-1-0:presentationURL "/")
             ;; removed iconList
             (urn:schemas-upnp-org:device-1-0:serviceList
              (urn:schemas-upnp-org:device-1-0:service
               (urn:schemas-upnp-org:device-1-0:serviceType
                "urn:schemas-upnp-org:service:ContentDirectory:1")
               (urn:schemas-upnp-org:device-1-0:serviceId
                "urn:upnp-org:serviceId:ContentDirectory")
               (urn:schemas-upnp-org:device-1-0:controlURL "/ctl/ContentDir")
               (urn:schemas-upnp-org:device-1-0:eventSubURL "/evt/ContentDir")
               (urn:schemas-upnp-org:device-1-0:SCPDURL "/ContentDir.xml"))
              (urn:schemas-upnp-org:device-1-0:service
               (urn:schemas-upnp-org:device-1-0:serviceType
                "urn:schemas-upnp-org:service:ConnectionManager:1")
               (urn:schemas-upnp-org:device-1-0:serviceId
                "urn:upnp-org:serviceId:ConnectionManager")
               (urn:schemas-upnp-org:device-1-0:controlURL "/ctl/ConnectionMgr?key=val:ue")
               (urn:schemas-upnp-org:device-1-0:eventSubURL "/evt/ConnectionMgr")
               (urn:schemas-upnp-org:device-1-0:SCPDURL "/ConnectionMgr.xml"))
              (urn:schemas-upnp-org:device-1-0:service
               (urn:schemas-upnp-org:device-1-0:serviceType
                "urn:microsoft.com:service:X_MS_MediaReceiverRegistrar:1")
               (urn:schemas-upnp-org:device-1-0:serviceId
                "urn:microsoft.com:serviceId:X_MS_MediaReceiverRegistrar")
               (urn:schemas-upnp-org:device-1-0:controlURL "/ctl/X_MS_MediaReceiverRegistrar")
               (urn:schemas-upnp-org:device-1-0:eventSubURL)
               (urn:schemas-upnp-org:device-1-0:SCPDURL "/X_MS_MediaReceiverRegistrar.xml")))))))

 (test "urn:schemas-upnp-org:device:MediaServer:1" (device-type doc))
 (test "Kristian Thinkpad" (friendly-name doc))

 (test "service-type"
       '("urn:schemas-upnp-org:service:ContentDirectory:1"
         "urn:schemas-upnp-org:service:ConnectionManager:1"
         "urn:microsoft.com:service:X_MS_MediaReceiverRegistrar:1")
       (map service-type (services doc)))

 (test "control-url"
       '("/ctl/ContentDir" "/ctl/ConnectionMgr?key=val:ue" "/ctl/X_MS_MediaReceiverRegistrar")
       (map control-url (services doc)))

 (test "absolute-control-url relative"
       '("http://host.com/ctl/ContentDir"
         "http://host.com/ctl/ConnectionMgr?key=val:ue"
         "http://host.com/ctl/X_MS_MediaReceiverRegistrar")
       (map (cut absolute-control-url "http://host.com" <>)
            (services doc)))

 (test "absolute-control-url absolute"
       "http://other-example.com/cd"
       (absolute-control-url "http://ignored-host.com/"
                             '(urn:schemas-upnp-org:device-1-0:service
                               (urn:schemas-upnp-org:device-1-0:serviceType
                                "urn:schemas-upnp-org:service:ContentDirectory:1")
                               (urn:schemas-upnp-org:device-1-0:serviceId
                                "urn:upnp-org:serviceId:ContentDirectory")
                               (urn:schemas-upnp-org:device-1-0:controlURL
                                "http://other-example.com/cd"))))

 (test "absolute-control-url no content-directory service"
       #f
       (absolute-control-url "http://abc" '() )))




(test-group
 "service-alist"

 (define doc
   '(*TOP* (*PI* xml "version=\"1.0\"")
           (urn:schemas-upnp-org:device-1-0:root
            (urn:schemas-upnp-org:device-1-0:specVersion
             (urn:schemas-upnp-org:device-1-0:major "1")
             (urn:schemas-upnp-org:device-1-0:minor "0"))
            (urn:schemas-upnp-org:device-1-0:URLBase "http://base/")
            (urn:schemas-upnp-org:device-1-0:device
             (urn:schemas-upnp-org:device-1-0:serviceList
              (urn:schemas-upnp-org:device-1-0:service
               (urn:schemas-upnp-org:device-1-0:serviceType "fw")
               (urn:schemas-upnp-org:device-1-0:controlURL "/fw")))
             (urn:schemas-upnp-org:device-1-0:deviceList
              (urn:schemas-upnp-org:device-1-0:device
               (urn:schemas-upnp-org:device-1-0:serviceList
                (urn:schemas-upnp-org:device-1-0:service
                 (urn:schemas-upnp-org:device-1-0:serviceType "lan")
                 (urn:schemas-upnp-org:device-1-0:controlURL "/lan"))))
              (urn:schemas-upnp-org:device-1-0:device
               (urn:schemas-upnp-org:device-1-0:serviceList
                (urn:schemas-upnp-org:device-1-0:service
                 (urn:schemas-upnp-org:device-1-0:serviceType "wan")
                 (urn:schemas-upnp-org:device-1-0:controlURL "/wan")))))))))

 (test "with base url from doc"
       '((fw  . "http://base/fw")
         (lan . "http://base/lan")
         (wan . "http://base/wan"))
       (service-alist doc))

 (test "with base from arg"
       '((fw . "/fw")
         (lan . "/lan")
         (wan . "/wan"))
       (service-alist doc "")))

;; ==================== windown mediaserver ====================
;; wanna test against this?
'(*doc* (*PI* xml "version=\"1.0\"")
        (urn:schemas-upnp-org:device-1-0:root
         (urn:schemas-upnp-org:device-1-0:specVersion
          (urn:schemas-upnp-org:device-1-0:major "1")
          (urn:schemas-upnp-org:device-1-0:minor "0"))
         (urn:schemas-upnp-org:device-1-0:device
          (urn:schemas-upnp-org:device-1-0:UDN
           "uuid:8981428a-a6bc-435c-9c99-ceb4e8d39829")
          (urn:schemas-upnp-org:device-1-0:modelName
           "Windows Media Player Sharing")
          (urn:schemas-upnp-org:device-1-0:modelNumber "12.0")
          (urn:schemas-upnp-org:device-1-0:serviceList
           (urn:schemas-upnp-org:device-1-0:service
            (urn:schemas-upnp-org:device-1-0:serviceType
             "urn:schemas-upnp-org:service:ConnectionManager:1")
            (urn:schemas-upnp-org:device-1-0:serviceId
             "urn:upnp-org:serviceId:ConnectionManager")
            (urn:schemas-upnp-org:device-1-0:controlURL
             "/upnphost/udhisapi.dll?control=uuid:8981428a-a6bc-435c-9c99-ceb4e8d39829+urn:upnp-org:serviceId:ConnectionManager")
            (urn:schemas-upnp-org:device-1-0:eventSubURL
             "/upnphost/udhisapi.dll?event=uuid:8981428a-a6bc-435c-9c99-ceb4e8d39829+urn:upnp-org:serviceId:ConnectionManager")
            (urn:schemas-upnp-org:device-1-0:SCPDURL
             "/upnphost/udhisapi.dll?content=uuid:33307824-ab68-4575-bc6e-cb8b376127a1"))
           (urn:schemas-upnp-org:device-1-0:service
            (urn:schemas-upnp-org:device-1-0:serviceType
             "urn:schemas-upnp-org:service:ContentDirectory:1")
            (urn:schemas-upnp-org:device-1-0:serviceId
             "urn:upnp-org:serviceId:ContentDirectory")
            (urn:schemas-upnp-org:device-1-0:controlURL
             "/upnphost/udhisapi.dll?control=uuid:8981428a-a6bc-435c-9c99-ceb4e8d39829+urn:upnp-org:serviceId:ContentDirectory")
            (urn:schemas-upnp-org:device-1-0:eventSubURL
             "/upnphost/udhisapi.dll?event=uuid:8981428a-a6bc-435c-9c99-ceb4e8d39829+urn:upnp-org:serviceId:ContentDirectory")
            (urn:schemas-upnp-org:device-1-0:SCPDURL
             "/upnphost/udhisapi.dll?content=uuid:ceb3b366-ca8d-4656-bceb-02df3cd50290"))
           (urn:schemas-upnp-org:device-1-0:service
            (urn:schemas-upnp-org:device-1-0:serviceType
             "urn:microsoft.com:service:X_MS_MediaReceiverRegistrar:1")
            (urn:schemas-upnp-org:device-1-0:serviceId
             "urn:microsoft.com:serviceId:X_MS_MediaReceiverRegistrar")
            (urn:schemas-upnp-org:device-1-0:controlURL
             "/upnphost/udhisapi.dll?control=uuid:8981428a-a6bc-435c-9c99-ceb4e8d39829+urn:microsoft.com:serviceId:X_MS_MediaReceiverRegistrar")
            (urn:schemas-upnp-org:device-1-0:eventSubURL
             "/upnphost/udhisapi.dll?event=uuid:8981428a-a6bc-435c-9c99-ceb4e8d39829+urn:microsoft.com:serviceId:X_MS_MediaReceiverRegistrar")
            (urn:schemas-upnp-org:device-1-0:SCPDURL
             "/upnphost/udhisapi.dll?content=uuid:152a554e-d8e7-4246-8afa-3ab833e459af"))))))

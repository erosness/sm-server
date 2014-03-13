
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
               (urn:schemas-upnp-org:device-1-0:controlURL "/ctl/ConnectionMgr")
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
       '("/ctl/ContentDir" "/ctl/ConnectionMgr" "/ctl/X_MS_MediaReceiverRegistrar")
       (map control-url (services doc)))

 (test "absolute-control-url relative"
       "http://example.com/ctl/ContentDir"
       (absolute-control-url "http://example.com"
                             doc))

 (test "absolute-control-url absolute"
       "http://other-example.com/cd"
       (absolute-control-url "http://ignored-host.com/"
                             '(*TOP*
                               (urn:schemas-upnp-org:device-1-0:root
                                (urn:schemas-upnp-org:device-1-0:device
                                 (urn:schemas-upnp-org:device-1-0:serviceList
                                  (urn:schemas-upnp-org:device-1-0:service
                                   (urn:schemas-upnp-org:device-1-0:serviceType
                                    "urn:schemas-upnp-org:service:ContentDirectory:1")
                                   (urn:schemas-upnp-org:device-1-0:serviceId
                                    "urn:upnp-org:serviceId:ContentDirectory")
                                   (urn:schemas-upnp-org:device-1-0:controlURL
                                    "http://other-example.com/cd")))))))))

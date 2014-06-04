

(test-group
 "unbox-result"
 (test "browse result"
       '(*TOP* (browse))
       (unbox-result '(*TOP* (*PI* xml "version=\"1.0\" encoding=\"utf-8\"")
                             (http://schemas.xmlsoap.org/soap/envelope/:Envelope
                              (http://schemas.xmlsoap.org/soap/envelope/:Body
                               (urn:schemas-upnp-org:service:ContentDirectory:1:BrowseResponse
                                (Result "<browse></browse>")))))))

 (test "search result"
       '(*TOP* (search))
       (unbox-result '(*TOP* (*PI* xml "version=\"1.0\" encoding=\"utf-8\"")
                             (http://schemas.xmlsoap.org/soap/envelope/:Envelope
                              (@ (http://schemas.xmlsoap.org/soap/envelope/:encodingStyle
                                  "http://schemas.xmlsoap.org/soap/encoding/"))
                              (http://schemas.xmlsoap.org/soap/envelope/:Body
                               (urn:schemas-upnp-org:service:ContentDirectory:1:SearchResponse
                                (Result
                                 "<search></search>"))))))))
(test-group
 "container sxml"


 (test "doc-tracks picks musicTracks only"
       '((urn:schemas-upnp-org:metadata-1-0/DIDL-Lite/:item
          (urn:schemas-upnp-org:metadata-1-0/upnp/:class
           "object.item.audioItem.musicTrack")))
       (doc-tracks
        '(*TOP* (urn:schemas-upnp-org:metadata-1-0/DIDL-Lite/:DIDL-Lite
                 ;; photoes are evil
                 (urn:schemas-upnp-org:metadata-1-0/DIDL-Lite/:item
                  (urn:schemas-upnp-org:metadata-1-0/upnp/:class
                   "object.item.imageItem.photo"))
                 (urn:schemas-upnp-org:metadata-1-0/DIDL-Lite/:item
                  (urn:schemas-upnp-org:metadata-1-0/upnp/:class
                   "object.item.audioItem.musicTrack"))))))
 (test
  "container->folder"
  '(container (id . "1$7")
              (title . "Album")
              (subtitle . "Artist")
              (image . "file.jpg"))
  (container->talist '(container
                       (@ (id "1$7"))
                       (http://purl.org/dc/elements/1.1/:title "Album")
                       (urn:schemas-upnp-org:metadata-1-0/upnp/:artist "Artist")
                       (urn:schemas-upnp-org:metadata-1-0/upnp/:albumArtURI "file.jpg"))))

 ;; this one should implicitly test container->talist and item->talist

 (test
  "containers->folders"
  '((container (id . "1$7") (title . "Album") (subtitle . "Artist") (image . "albumArtURI"))
    ;; important: missing subtitle, image etc don't produce fields with #f.
    (container (id . "1$6") (title . "Artist"))
    (track (id . "tid")
           (artist . "artist")
           (album . "album")
           (title . "title")
           (turi . "tr://example/tid")))
  (didl->talist
   '(*TOP* (urn:schemas-upnp-org:metadata-1-0/DIDL-Lite/:DIDL-Lite
            (urn:schemas-upnp-org:metadata-1-0/DIDL-Lite/:container
             (@ (id "1$7"))
             (http://purl.org/dc/elements/1.1/:title "Album")
             (urn:schemas-upnp-org:metadata-1-0/upnp/:artist "Artist")
             (urn:schemas-upnp-org:metadata-1-0/upnp/:albumArtURI "albumArtURI"))
            (urn:schemas-upnp-org:metadata-1-0/DIDL-Lite/:container
             (@ (id "1$6"))
             (http://purl.org/dc/elements/1.1/:title "Artist"))
            (urn:schemas-upnp-org:metadata-1-0/DIDL-Lite/:item
             (@ (id "tid"))
             (urn:schemas-upnp-org:metadata-1-0/upnp/:class "object.item.audioItem.musicTrack")
             (http://purl.org/dc/elements/1.1/:title "title")
             (urn:schemas-upnp-org:metadata-1-0/upnp/:artist "artist")
             (urn:schemas-upnp-org:metadata-1-0/upnp/:album "album")
             (urn:schemas-upnp-org:metadata-1-0/upnp/:albumArtURI "http://albums.com/")
             (urn:schemas-upnp-org:metadata-1-0/DIDL-Lite/:res "tr://example/tid")))))))

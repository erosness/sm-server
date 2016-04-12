
(test-group
 "parse-duration"
 (define (min . args) (fold + 0 (map (cut * <> 60) args)))
 (define (hour . args) (fold + 0 (map (cut * <> 60 60) args)))
 (define (sec . args) (fold + 0 args))
 (test "simplest case" (sec (hour 1) (min 2) 3) (parse-duration "1:2:3"))
 (test "00 format" (sec (hour 1) (min 2) 3.5) (parse-duration "01:02:03.5"))
 (test "0" (sec (hour 2) (min 3) 4.5) (parse-duration "2:3:4.5"))

 (test "only seconds" 12345.25 (parse-duration "12345.25"))
 (test "minutes:seconds" (sec (min 10) 3) (parse-duration "10:3"))
 (test "garbage" #f (parse-duration "foo")))


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
  "track->talist with minidlna snippet"
  `(track (id . "x")
          (turi . "http://host/file.mp3")
          (title . "title")
          (subtitle . "Michael Buble")
          (album . "With Love")
          (image . "cover.jpg"))
  (track->talist
   `(urn:schemas-upnp-org:metadata-1-0/DIDL-Lite/:item
     (@ (id "x"))
     (http://purl.org/dc/elements/1.1/:title "title")
     (urn:schemas-upnp-org:metadata-1-0/upnp/:class "object.item.audioItem.musicTrack")
     (urn:schemas-upnp-org:metadata-1-0/upnp/:artist "Michael Buble")
     (urn:schemas-upnp-org:metadata-1-0/upnp/:album "With Love")
     (urn:schemas-upnp-org:metadata-1-0/upnp/:albumArtURI "cover.jpg")
     (urn:schemas-upnp-org:metadata-1-0/DIDL-Lite/:res "http://host/file.mp3"))))

 (test "track->talist duration"
       `(track (id . "ID") (turi . "file://turi") (duration . ,(+ 4.5 (* 60 (+ 3 (* 60 2))))))
       (track->talist
        '(urn:schemas-upnp-org:metadata-1-0/DIDL-Lite/:item
          (@ (id "ID"))
          (urn:schemas-upnp-org:metadata-1-0/DIDL-Lite/:res
           (@ (duration "2:3:4.5"))
           "file://turi"))))

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
           (turi . "tr://example/tid")
           (title . "title")
           (subtitle . "artist")
           (album . "album")
           (image . "http://albums.com/")))
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

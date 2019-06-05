;;; DIDL: Digital Item Declaration Language
;;;
;;; Reparse search/browse results to more manageable talists.
;;; DIDL-Lite XML is used in UPnP/DLNA server responses. It's an XML
;;; for listing containers (folder) and tracks (items).
;;;
;;; What's ironic is that UPnP seems to have its own standard for
;;; artists, songs, albums etc. So DIDL is just container or item, no?
(use ssax sxpath sxml-serializer
     clojurian-syntax
     http-client)

(include "sxml-common.scm")
(include "header-compatibility.scm")

;; ==================== didl processing ====================

;; convert verbose container-sxml into blurp. (type "id" "title" attr ...)
(define (container->talist container)

  (define (r field sxpathproc)
    (let ((result (sxpathproc container)))
      (if result `((,field . ,result)) '())))

  (define container-title  (sxpath/car "pe:title/text()" ns))
  (define container-id     (sxpath "string(@id)" ns))
  (define container-artist (sxpath/car "u:artist/text()" ns))
  (define container-image  (sxpath/car "u:albumArtURI/text()" ns))
  `(container ,@(r 'id container-id)
              ,@(r 'title container-title)
              ,@(r 'subtitle container-artist)
              ,@(r 'image container-image)))

(use irregex)
;; parse to match "((hh:mm:)|(mm:)|)ss". returns #f if unable to
;; parse.
(define (parse-duration str)
  (cond ((irregex-match `(: (or (: (=> hh (* digit)) ":" (=> mm (* digit)) ":")
                                (: (=> mm (* digit)) ":")
                                (:))
                            (=> ss (* digit) (? "." (* digit))))
                        str)
         =>
         (lambda (m)
           (define (match name)
             (or (string->number (or (irregex-match-substring m name) "")) 0))
           (+ (match 'ss)
              (* 60 (+ (match 'mm)
                       (* 60 (match 'hh)))))))
        (else #f)))

;; convert an sxml item into pretty blurps
(define (track->talist item)
  (define (r field proc)
    (let ((result (proc item)))
      (if result `((,field . ,result)) `())))

  (define item-id     (sxpath     "string(@id)" ns))
  (define item-title  (sxpath/car "pe:title/text()" ns))
  (define item-artist (sxpath/car "u:artist/text()" ns))
  (define item-album  (sxpath/car "u:album/text()" ns))
  (define item-image  (sxpath/car "u:albumArtURI/text()" ns))
  (define item-turi   (sxpath/car "d:res/text()" ns))
  (define (item-duration item)
    (define duration ((sxpath/car "d:res/@duration/text()" ns) item))
    (if (string? duration) (parse-duration duration) #f))

  `(track ,@(r 'id       item-id)
          ,@(r 'turi     item-turi)
          ,@(r 'type     (lambda (x) "dlna" ))
          ,@(r 'title    item-title)
          ,@(r 'subtitle item-artist)
          ,@(r 'album    item-album)
          ,@(r 'duration item-duration)
          ,@(r 'image    item-image)))

(define doc-containers (sxpath "//d:DIDL-Lite/d:container" ns))
(define doc-tracks (sxpath
                    (conc "//d:DIDL-Lite/d:item" ;; any item
                          "[u:class/text()" ;; with <class> containing musicTrack
                          "[contains(.,'object.item.audioItem.musicTrack')]"
                          "]") ns))

;; convert disgusting sxml response into pretty blurps.
(define (didl->talist doc)
  (append (map container->talist (doc-containers doc))
          (map track->talist     (doc-tracks doc))))



;; ==================== net ====================

;; soap wraps the response payload in XML. The response payload is
;; therefore escaped XML inside XML. So we need to parse the XML
;; twice. Who says SOAP is complex?
;;
;; pick out the raw result body and returns it as sxml.
(define (unbox-result doc)

  ;; sometimes it's here, sometimes it's there. find them both and
  ;; return it all.
  (define sr "//e:Envelope/e:Body/cd1:SearchResponse/Result/text()")
  (define br "//e:Envelope/e:Body/cd1:BrowseResponse/Result/text()")

  (let* ((strs (append ((sxpath sr ns) doc)
                       ((sxpath br ns) doc)))
         (str (if (= 1 (length strs))
                  (car strs)
                  (error (conc "unbox error (found " (length strs) ")")
                         doc))))
    (ssax:xml->sxml (open-input-string str) '())))


;; query host for children/subfolders of container-id. returns sxml.
(define (sxml-query request sxml)
  (->> (ssax:xml->sxml (open-input-string
                        (with-soap-unparsers
                         (lambda ()
                           (with-input-from-request
                            request
                            (serialize-sxml sxml)
                            read-string))))
                       '())
       (unbox-result)))


;; content-type and soapaction stolen from Coherence's UPnP inspector
(define (make-soap-request soapaction url)
  (->> `((content-type "text/xml ;charset=\"utf-8\"")
         (soapaction ,soapaction))
       (headers)
       (make-request uri: (uri-reference url)
                     method: 'POST
                     major: 1 minor: 0
                     headers: )))


;; ==================== browse ====================

;; construct sxml for media-browse request
(define (browse/sxml parent-id)
  `(*TOP* (@ (*NAMESPACES*
              (e "http://schemas.xmlsoap.org/soap/envelope/")
              (cd1 "urn:schemas-upnp-org:service:ContentDirectory:1")))
          (*PI* xml "version=\"1.0\" encoding=\"utf-8\"")
          (e:Envelope
           (@ (e:encodingStyle "http://schemas.xmlsoap.org/soap/encoding/"))
           (e:Body
            (cd1:Browse
             (ObjectID ,(conc parent-id))
             (BrowseFlag "BrowseDirectChildren")
             (Filter "*")
             (StartingIndex "0")
             (RequestedCount "0")
             (SortCriteria))))))


;; ==================== search ====================

;; query is e.g.
;; "(upnp:class = \"object.container.album.musicAlbum\" and dc:title contains \"lara\")"
(define (search/sxml query parent)
  `(*TOP* (*PI* xml "version=\"1.0\" encoding=\"utf-8\" standalone=\"yes\"")
          (http://schemas.xmlsoap.org/soap/envelope/:Envelope
           (@ (http://schemas.xmlsoap.org/soap/envelope/:encodingStyle
               "http://schemas.xmlsoap.org/soap/encoding/"))
           (http://schemas.xmlsoap.org/soap/envelope/:Body
            (urn:schemas-upnp-org:service:ContentDirectory:1:Search
             (ContainerID ,parent)
             (SearchCriteria ,query)
             (Filter "*")
             (StartingIndex "0")
             (RequestedCount "16")
             (SortCriteria))))))


(define (browse-query url container-id)
  (sxml-query (make-soap-request "urn:schemas-upnp-org:service:ContentDirectory:1#Browse" url)
              (browse/sxml container-id)))

(define (search-query url query #!optional (parent "0"))
  (sxml-query (make-soap-request "urn:schemas-upnp-org:service:ContentDirectory:1#Search" url)
              (search/sxml query parent)))

(include "didl.test.scm")

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
  (define container-title (sxpath/car "pe:title/text()" ns))
  (define container-id    (sxpath "string(@id)" ns))
  `(container (id . ,(container-id container))
              (title . ,(container-title container))))

;; convert an sxml item into pretty blurps
(define (item->talist item)
  (define item-id     (sxpath     "string(@id)" ns))
  (define item-title  (sxpath/car "pe:title/text()" ns))
  (define item-artist (sxpath/car "u:artist/text()" ns))
  (define item-album  (sxpath/car "u:album/text()" ns))
  (define item-turi   (sxpath/car "d:res/text()" ns))
  `(track (id     . ,(item-id item))
          (artist . ,(item-artist item))
          (album  . ,(item-album item))
          (title  . ,(item-title item))
          (turi   . ,(item-turi item))))

;; convert disgusting sxml response into pretty blurps.
(define (didl->talist doc)
  (append (map container->talist ((sxpath "//d:DIDL-Lite/d:container" ns) doc))
          (map item->talist      ((sxpath "//d:DIDL-Lite/d:item" ns) doc))))


;; ==================== browse ====================

;; construct sxml for media-browse request
(define (browse-children/sxml parent-id)
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

;; content-type and soapaction stolen from Coherence's UPnP inspector
;; (note the quotes, see with-soap-unparsers)
(define (make-soap-request soapaction url)
  (->> `((content-type "text/xml ;charset=\"utf-8\"")
         (soapaction ,soapaction))
       (headers)
       (make-request uri: (uri-reference url)
                     method: 'POST
                     major: 1 minor: 0
                     headers: )))

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

(define (browse-query url container-id)
  (sxml-query (make-soap-request "urn:schemas-upnp-org:service:ContentDirectory:1#Browse" url)
              (browse-children/sxml container-id)))

;; ==================== search ====================
;; TODO

(include "didl.test.scm")



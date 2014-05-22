(module rest-wimp ()

(import chicken scheme data-structures)
(use wimp uri-common test clojurian-syntax restlib
     matchable)

(import rest turi)

;; leaving the beauty of what is session management till later:
(print "please wait while logging in to wimp...")
(wimp-login! "97670550" "herrowimp")

;; ==================== audio host ====================
;; (e.g convert tr://10.0.0.22/ah/wimp/tid/1234
;;         => https://api.stream.wimp.com/dwvpqm7xh)


(define (tid->suri tid)
  (alist-ref 'url (wimp-track-streamurl tid)))

;; OBS: returning WIMP's metadata directly (and assuming it's url
;; field is the http url for the streaming url).
(define (play-command/wimp tid)
  (wimp-track-streamurl tid))

(define-turi-adapter tid->turi "wimp" (lambda (id) (play-command/wimp id)))

(define (track->turi track)
  (tid->turi (or (alist-ref 'id track)
                 (error "no id field for track" track))))

(test-group
 "turi conversion"
 (with-request
  ("/" `((host ("host" . 1))))
  (test "path->url"   "tr://host:1/v1/t2s?type=wimp&id=x" (tid->turi "x"))
  (test "track->turi" "tr://host:1/v1/t2s?type=wimp&id=123" (track->turi `((id . "123"))))))

;; (with-request "?type=wimp&id=1234" (/t2s))

;; ==================== browsing ====================
;; ************ image getters
(define (track->album-cover-uri track)
  (let ((aid (->> track
                  (alist-ref 'album)
                  (alist-ref 'id))))
    (wimp-album-cover-url aid)))

(define (album->album-cover-uri album)
  (wimp-album-cover-url (alist-ref 'id album)))

(define (artist->artist-image-uri artist)
  (wimp-artist-image-url (alist-ref 'id artist)))



(define (track/album->artist-name item)
  (->> item
       (alist-ref 'artist)
       (alist-ref 'name)))


;; *********** ->search-result

(define (track->search-result track)
  `((turi   . ,(track->turi track))
    (title  . ,(alist-ref 'title track))
    (subtitle . ,(track/album->artist-name track))
    (image  . ,(track->album-cover-uri track))))

(define (artist->search-result artist)
  `((id    . ,(alist-ref 'id artist))
    ;; This was changed to make the artist/track/album results uniform for
    ;; the client.
    ;; TODO: Properly define what a result should look like
    ;; -    (name  . ,(alist-ref 'name artist))
    ;; -    (image . ,(artist->artist-image-uri artist))))
    (title  . ,(alist-ref 'name artist))
    (image  . ,(artist->artist-image-uri artist))
    (uri    . ,(conc "/catalog/wimp/artist/albums?artist=" (alist-ref 'id artist)))))

(define (album->search-result album)
  `((id     . ,(alist-ref 'id album))
    (subtitle . ,(track/album->artist-name album))
    (title  . ,(alist-ref 'title album))
    (image  . ,(album->album-cover-uri album))
    (uri    . ,(conc "/catalog/wimp/album/tracks?album=" (alist-ref 'id album)))))


(define (wimp-process-result result-proc result)
  (map result-proc
       (->> result
            (alist-ref 'items)
            (vector->list))))

(define ((make-wimp-search-call search process) q #!optional (limit 10) (offset 0))
  (let ((result (search q `((offset . ,offset)
                            (limit  . ,limit)))))
    (make-search-result limit offset
                        (alist-ref 'totalNumberOfItems result)
                        (wimp-process-result process result))))


(define (wrap-wimp search-proc convert #!optional (query-param 'q))
  (argumentize
   (make-wimp-search-call search-proc convert)
   query-param '(limit "10") '(offset "0")))

;;==================== handlers ====================
(define-handler /catalog/wimp
  (lambda () `((tabs . #( ((title . "Artists") (uri . "/catalog/wimp/artist"))
                     ((title . "Albums")  (uri . "/catalog/wimp/album"))
                     ((title . "Tracks")  (uri . "/catalog/wimp/track")))))))

(define-handler /catalog/wimp/track         (wrap-wimp wimp-search-track  track->search-result))
(define-handler /catalog/wimp/album         (wrap-wimp wimp-search-album  album->search-result))
(define-handler /catalog/wimp/artist        (wrap-wimp wimp-search-artist artist->search-result))

(define-handler /catalog/wimp/artist/albums
  (wrap-wimp wimp-artist-albums album->search-result 'artist))

(define-handler /catalog/wimp/album/tracks
  (wrap-wimp wimp-album-tracks track->search-result 'album))

;; ==================== tests ====================


(use wimp uri-common test irregex intarweb spiffy)

;; whenever wimp tries to make a request, pick out its request uri and
;; make a brutal exit, returning its value. no network or
;; post-processing will be performed on the uri.
(define-syntax return-wimp-uri
  (syntax-rules ()
    ((_ call)
     (call/cc
      (lambda (return)
        (parameterize ((*wimp-query* (lambda (uri a b) (return (uri->string uri)))))
          call))))))


(test-group
 "wimp-rest"

 (test "artist/albums"
       #t
       (->> (/catalog/wimp/artist/albums)
            (return-wimp-uri)
            (with-request "?artist=1234")
            (irregex-search "/artists/1234")
            ((conjoin identity))))

 (test "artist/albums"
       #t
       (->> (/catalog/wimp/album/tracks)
            (return-wimp-uri)
            (with-request "?album=9876")
            (irregex-search "/albums/9876")
            ((conjoin identity)))))

)

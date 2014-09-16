(module rest-wimp (clear-wimp-session!)

(import chicken scheme data-structures)
(use wimp uri-common test clojurian-syntax restlib
     medea
     matchable)

(import turi
        rest ;; <-- return-url
        store)

(define (do-wimp-login store)
  (or (and-let* (((list? store))
                 (username (alist-ref 'username store))
                 (password (alist-ref 'password store)))
        (print "please wait while logging in to wimp...")
        (wimp-login! username password))
      (print "wimp - no login credentials found")))

(define wimp-store (make-store 'wimp '()))
(condition-case
    (do-wimp-login (wimp-store))
  ((exn) (print "wimp - login failed")))

;; ==================== audio host ====================
;; (e.g convert tr://10.0.0.22/ah/wimp/tid/1234
;;         => https://api.stream.wimp.com/dwvpqm7xh)


(define (tid->suri tid)
  (alist-ref 'url (wimp-track-streamurl tid)))

;; OBS: returning WIMP's metadata directly (and assuming it's url
;; field is the http url for the streaming url).
(define (play-command/wimp tid)
  ;; need to return 1 value only (second would be HTTP status-code)
  (values (wimp-track-streamurl tid)))

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
    (available . ,(alist-ref 'allowStreaming track))
    (duration . ,(alist-ref 'duration track))
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
    (uri    . ,(return-url "/catalog/wimp/artist/albums?artist=" (alist-ref 'id artist)))))

(define (album->search-result album)
  `((id     . ,(alist-ref 'id album))
    (subtitle . ,(track/album->artist-name album))
    (title  . ,(alist-ref 'title album))
    (image  . ,(album->album-cover-uri album))
    (uri    . ,(return-url "/catalog/wimp/album/tracks?album=" (alist-ref 'id album)))))

(define (playlist->search-result playlist)
  `((title . ,(alist-ref 'title playlist))
    (uri   . ,(return-url "/catalog/wimp/playlist/tracks?uuid=" (alist-ref 'uuid playlist)))))

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




(define (wrap-wimp-login-status handler)

  ;; extras should be an alist
  (define (make-wimp-login-error #!optional (extras '()))
    (values `((service . "wimp")
              (url . ,(return-url "/catalog/wimp/login"))
              (_debug . ((wimp-store
                          . ((username . ,(alist-ref 'username (wimp-store)))))))
              ,@extras)
            'unauthorized))

  (lambda ()
    (condition-case
     (handler)

     ;; this happens when a login to wimp services errors. convert to
     ;; nice 401 respons with json.
     (e (exn http client-error)
        ;; login failed. return 401 and wimp's unmodified json response
        ;; (found in the exception message as a string).
        (make-wimp-login-error
         `((wimp-response . ,(read-json
                              ((condition-property-accessor
                                'client-error
                                'body) e))))))

     ;; this happens when wimp egg doesn't have any credentials
     ;; stored:
     (e (wimp missing-login) (make-wimp-login-error)))))

(define (wrap-wimp search-proc convert #!optional (query-param 'q))
  (wrap-wimp-login-status
   (argumentize
    (make-wimp-search-call search-proc convert)
    query-param '(limit "10") '(offset "0"))))

(define (wimp-user-playlists/current-user _ignored_ query-params)
  (wimp-user-playlists (wimp-current-user-id) query-params))

;;==================== handlers ====================
(define-handler /v1/catalog/wimp
  (wrap-wimp-login-status
   (lambda () `((search . #( ((title . "Artists") (uri . ,(return-url "/catalog/wimp/artist")))
                      ((title . "Albums")  (uri . ,(return-url "/catalog/wimp/album")))
                      ((title . "Tracks")  (uri . ,(return-url "/catalog/wimp/track")))))
           (preload . #( ((title . "Playlists") (uri . ,(return-url "/catalog/wimp/playlists")))))
           ;; TODO: remove
           (tabs . #( ((title . "Artists") (uri . ,(return-url "/catalog/wimp/artist")))
                      ((title . "Albums")  (uri . ,(return-url "/catalog/wimp/album")))
                      ((title . "Tracks")  (uri . ,(return-url "/catalog/wimp/track")))))))))

(define-handler /v1/catalog/wimp/track         (wrap-wimp wimp-search-track  track->search-result))
(define-handler /v1/catalog/wimp/album         (wrap-wimp wimp-search-album  album->search-result))
(define-handler /v1/catalog/wimp/artist        (wrap-wimp wimp-search-artist artist->search-result))

;; falls apart: no query parameter anymore
(define-handler /v1/catalog/wimp/playlists
  (wrap-wimp wimp-user-playlists/current-user playlist->search-result
             '(uid . ,(lambda () (print "i wish defaults in argumentize could be dynamic")))))

(define-handler /v1/catalog/wimp/artist/albums
  (wrap-wimp wimp-artist-albums album->search-result 'artist))

(define-handler /v1/catalog/wimp/album/tracks
  (wrap-wimp wimp-album-tracks track->search-result 'album))

(define-handler /v1/catalog/wimp/playlist/tracks
  (wrap-wimp wimp-playlist-tracks track->search-result 'uuid))

(define-handler /v1/catalog/wimp/login
  (wrap-wimp-login-status
   (lambda ()
     (if (current-json)
         (or (and-let* ((username (alist-ref 'username (current-json)))
                        (password (alist-ref 'password (current-json))))
               (let ((result (do-wimp-login (current-json))))
                 (wimp-store (current-json))
                 result))
             ;; couldn't find login credentials in request
             (error "expected username and password keys in body " (current-json)))
         '())

     ;; return current login status
     `((user . ,(if *wimp-session-params*
                    (alist-ref 'username (wimp-store))
                    #f))))))

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


(if *wimp-session-params*
 (test-group
  "wimp-rest"

  (test "artist/albums"
        #t
        (->> (/v1/catalog/wimp/artist/albums)
             (return-wimp-uri)
             (with-request "?artist=1234")
             (irregex-search "/artists/1234")
             ((conjoin identity))))

  (test "artist/albums"
        #t
        (->> (/v1/catalog/wimp/album/tracks)
             (return-wimp-uri)
             (with-request "?album=9876")
             (irregex-search "/albums/9876")
             ((conjoin identity))))))

;; for debugging purposes
(define (clear-wimp-session!)
  (set! *wimp-session-params*
        `((sessionId . "00000000-0000-0000-0000-000000000000")
          (countryCode . "NO")))
  (print "wimp session cleared!"))

)

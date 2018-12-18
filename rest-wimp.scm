(module rest-wimp (clear-wimp-sessions!)

(import chicken scheme data-structures)
(use wimp uri-common test clojurian-syntax restlib
     medea
     matchable srfi-18 srfi-1)

(import turi
        rest ;; <-- return-url
        concurrent-utils)

(define (do-wimp-login store)
  (or (and-let* (((list? store))
                 (username (alist-ref 'username store))
                 (password (alist-ref 'password store)))
        (print "please wait while logging in to wimp...")
        (wimp-login username password))
      (print "wimp - no login credentials found")))


;; ============= sessions ====================
(define wimp-sessions '())
(define wimp-sessions-mutex (make-mutex))

(define (wimp-add-session username session)
  ((with-mutex-lock
    wimp-sessions-mutex
    (lambda ()
      (set! wimp-sessions
            (alist-update username session
                          wimp-sessions equal?))))))

(define (wimp-get-session username)
  (alist-ref username wimp-sessions equal?))

(fluid-let ((wimp-sessions '()))
  (test-group
   "wimp-sessions"
   (wimp-add-session "foo" "bar")
   (test
    "set/get"
    "bar" (wimp-get-session "foo"))
   (wimp-add-session "foo" "baz")
   (test
    "overwrites old"
    "baz" (wimp-get-session "foo"))
   (test
    "old was removed"
    1 (length wimp-sessions))))

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


(define-turi-adapter tid->turi
  "wimp" (lambda (params)
           (or (and-let* ((id (alist-ref 'id params))
                          (username (alist-ref 'username params)))

                 (parameterize ((current-session-params (wimp-get-session username)))
                   (play-command/wimp id)))

               (error "expecting id and username params, got " params))))


(define current-wimp-user (make-parameter #f))
(define (track->turi track)
  (and-let* ((id (or (alist-ref 'id track)
                     (error "no id field for track " track)))
             (username (or (current-wimp-user)
                           (error "expected current-wimp-user to be set"))))
    (tid->turi `((id . ,id) (username . ,username)))))

(test-group
 "turi conversion"
 (parameterize ((current-wimp-user "foo"))
  (with-request
   ("/" `((host ("host" . 1))))
   ;; HACK: host is renamed to 127.0.0.1 by issue #93
   (test "path->url"   "tr://127.0.0.1:1/v1/t2s?type=wimp&id=x" (tid->turi '((id . "x"))))
   (test "track->turi" "tr://127.0.0.1:1/v1/t2s?type=wimp&id=123&username=foo" (track->turi `((id . "123")))))))

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
    (type . "wimp")
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
    (image . ,(tidal-playlist-image-url (alist-ref 'image playlist)))
    (uri   . ,(return-url "/catalog/wimp/playlist/tracks?uuid=" (alist-ref 'uuid playlist)))))

(define (mood->search-result mood)
  `((title . ,(alist-ref 'name mood))
    (image . ,(tidal-mood-image-url (alist-ref 'image mood)))
    (uri   . ,(return-url "/catalog/wimp/moods?mood=" (alist-ref 'path mood)))))

(define (genre->search-result genre)
  `((title . ,(alist-ref 'name genre))
    (image . ,(tidal-mood-image-url (alist-ref 'image genre)))
    (uri   . ,(return-url "/catalog/wimp/genres/albums?genre=" (alist-ref 'path genre)))))

(define (wimp-process-result result-proc result)
  (map result-proc
       (->> result
            (alist-ref 'items)
            (vector->list))))


;; TODO: refactor make q optinal and get rid of make-wimp-get-call
(define ((make-wimp-search-call search process) q username #!optional (limit 10) (offset 0))
  (parameterize ((current-wimp-user username)
                 (current-session-params (wimp-get-session username)))
    (let ((result (search q `((offset . ,offset)
                              (limit  . ,limit)))))
      (make-search-result limit offset
                          (alist-ref 'totalNumberOfItems result)
                          (wimp-process-result process result)))))

(define ((make-wimp-get-call get process) username #!optional (limit 10) (offset 0))
  (parameterize ((current-wimp-user username)
                 (current-session-params (wimp-get-session username)))
    (let ((result (get `((offset . ,offset)
                         (limit  . ,limit)))))
      (make-search-result limit offset
                          (alist-ref 'totalNumberOfItems result)
                          (wimp-process-result process result)))))


(define (wrap-wimp-login-status handler)

  ;; extras should be an alist
  (define (make-wimp-login-error #!optional (extras '()))
    (values `((service . "wimp")
	      
              (url . ,(return-url "/catalog/wimp/login"))
              (_debug . ((session . ,(current-session-params))))
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


;; TODO: refactor wrap-wimp and wrap-wimp-no-q
(define (wrap-wimp search-proc convert #!optional (query-param 'q))
  (wrap-wimp-login-status
   (argumentize
    (make-wimp-search-call search-proc convert)
    query-param 'username '(limit "10") '(offset "0"))))

(define (wrap-wimp-no-q search-proc convert)
  (wrap-wimp-login-status
   (argumentize
    (make-wimp-get-call search-proc convert)
    'username '(limit "10") '(offset "0"))))

(define (wimp-user-playlists/current-user _ignored_ query-params)
  (wimp-user-playlists (alist-ref 'userId (current-session-params)) query-params))


;; -------------
;; Favorites handler common code

;; Returns the userId of the current session or an empty string if no
;; current session. We return the empty string instead of #f to get
;; make-wimp-login-error trigger. With #f a chicken stack trace is
;; returned to the user instead.
(define (current-session-userid)
  (or (and (current-session-params)
           (alist-ref 'userId (current-session-params)))
      ""))

;; Create a handler for the favorites end-points
;; The conversion is slightly differnt here because the enpoint
;; returns a list of objects with two keys; 'item' and 'created' in
;; the 'items' array, instead of the contents of 'item' as the others do.
(define (make-wimp-favorites-handler search converter)
  (wrap-wimp-login-status
   (argumentize
    (lambda (username limit offset)
      (parameterize ((current-wimp-user username)
                     (current-session-params (wimp-get-session username)))
		    (let ((result (search (current-session-userid)
					  `((limit . ,limit ) (offset . ,offset )) )))
          (make-search-result limit offset
                              (alist-ref 'totalNumberOfItems result)
                              (map converter (map (lambda (x) (alist-ref 'item x))
                                                  (vector->list (alist-ref 'items result))))))))
    'username '(limit "10") '(offset "0"))))


;;==================== handlers ====================
(define-handler /v1/catalog/wimp
  (wrap-wimp-login-status
   (lambda () `((search . #( ((title . "Artists") (uri . ,(return-url "/catalog/wimp/artist")))
                        ((title . "Albums")  (uri . ,(return-url "/catalog/wimp/album")))
                        ((title . "Tracks")  (uri . ,(return-url "/catalog/wimp/track")))))
           (preload . #( ((title . "Playlists") (uri . ,(return-url "/catalog/wimp/playlists")))
                         ((title . "Moods") (uri . ,(return-url "/catalog/wimp/moods")))
                         ((title . "Discover") (uri . ,(return-url "/catalog/wimp/discover")))
                         ((title . "New") (uri . ,(return-url "/catalog/wimp/new")))
                         ((title . "Genres") (uri . ,(return-url "/catalog/wimp/genres")))
                         ((title . "Favorites") (uri . ,(return-url "/catalog/wimp/favorites")))))))))

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

(define-handler /v1/catalog/wimp/moods
  (wrap-wimp-login-status
   (pagize
    (argumentize (lambda (username mood)
                   (parameterize ((current-session-params (wimp-get-session username)))

                     (match mood
                       ("" (let ((res (wimp-editorial-moods)))
                             (map mood->search-result (vector->list res))))
                       (else (let ((res (wimp-editorial-moods-playlists mood)))
                               (wimp-process-result playlist->search-result res))))))

                 'username '(mood "")))))



(define-handler /v1/catalog/wimp/discover
  (pagize
   (lambda () `( ((title . "Discover Tracks")
             (uri   . ,(return-url "/catalog/wimp/discover/tracks")))
            ((title . "Discover Albums")
             (uri   . ,(return-url "/catalog/wimp/discover/albums")))))))



(define-handler /v1/catalog/wimp/discover/tracks
  (wrap-wimp-no-q wimp-editorial-discovery-tracks track->search-result))
(define-handler /v1/catalog/wimp/discover/albums
  (wrap-wimp-no-q wimp-editorial-discovery-albums album->search-result))

(define-handler /v1/catalog/wimp/new
  (pagize
   (lambda () `( ((title . "New Tracks")
             (uri   . ,(return-url "/catalog/wimp/new/tracks")))
            ((title . "New Albums")
             (uri   . ,(return-url "/catalog/wimp/new/albums")))
            ((title . "New Playlists")
             (uri   . ,(return-url "/catalog/wimp/new/playlists")))))))

(define-handler /v1/catalog/wimp/new/tracks
  (wrap-wimp-no-q wimp-editorial-featured-new-tracks track->search-result))
(define-handler /v1/catalog/wimp/new/albums
  (wrap-wimp-no-q wimp-editorial-featured-new-albums album->search-result))
(define-handler /v1/catalog/wimp/new/playlists
  (wrap-wimp-no-q wimp-editorial-featured-new-playlists playlist->search-result))

(define-handler /v1/catalog/wimp/genres
  (wrap-wimp-login-status
   (pagize
    (argumentize (lambda (username)
                   (parameterize ((current-session-params (wimp-get-session username)))
                     (let* ((res (wimp-genres))
                            (albums (filter (lambda (x) (alist-ref 'hasAlbums x)) (vector->list res))))
                       (map genre->search-result albums))))
                 'username))))

(define-handler /v1/catalog/wimp/genres/albums
  (wrap-wimp wimp-genres-albums album->search-result 'genre))

(define-handler /v1/catalog/wimp/login
  (wrap-wimp-login-status
   (lambda ()
     (if (current-json)
         (or (and-let* ((username (alist-ref 'username (current-json)))
                        (password (alist-ref 'password (current-json))))
               (let ((result (do-wimp-login (current-json))))
                 (wimp-add-session username result)
                 result))
             ;; couldn't find login credentials in request
             (error "expected username and password keys in body " (current-json)))
         ;; GET
         `((user . ,(and-let* ((username (current-query-param 'username)))
                      (wimp-get-session username))))))))

;; Favorites
(define-handler /v1/catalog/wimp/favorites
  (pagize
   (lambda () `( ((title . "Albums")
             (uri   . ,(return-url "/catalog/wimp/favorites/albums")))
            ((title . "Artists")
             (uri   . ,(return-url "/catalog/wimp/favorites/artists")))
            ((title . "Tracks")
             (uri   . ,(return-url "/catalog/wimp/favorites/tracks")))))))

(define-handler /v1/catalog/wimp/favorites/albums
  (make-wimp-favorites-handler wimp-favorites-albums album->search-result))
(define-handler /v1/catalog/wimp/favorites/artists
  (make-wimp-favorites-handler wimp-favorites-artists artist->search-result))
(define-handler /v1/catalog/wimp/favorites/tracks
  (make-wimp-favorites-handler wimp-favorites-tracks track->search-result))


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
(define (clear-wimp-sessions!)
  (set! wimp-sessions '())
  (print "wimp sessions cleared!"))

)

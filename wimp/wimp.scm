(use http-client uri-common medea
     clojurian-syntax
     srfi-1 srfi-18 srfi-13 files)



;; typically holds current sessionId and countryCode
(define *wimp-base-url*     (make-parameter "https://api.tidal.com/v1"))
(define *wimp-query*        (make-parameter with-input-from-request))
(define current-session-params (make-parameter #f))
(define *wimp-session-params* #f)

(define tidal-token "57QOvPeJ87DeB0J4")

;; most servers don't allow ; as query separator, even though it's in
;; the uri-specification. set "&" first so that we use that when
;; concatenating query parameters.
(form-urlencoded-separator "&;")

;; (use test)
;; (test '((a . 1) (d . 4)) (select-keys '((a . 1) (b . 2) (c . 3) (d . 4)) '(a d)))
(define (select-keys alist keys)
  (let loop ((s alist)
             (r '()))
    (if (null? s)
        (reverse r)
        (if (memq (caar s) keys)
            (loop (cdr s) (cons (car s) r))
            (loop (cdr s) r)))))

(define (append-urls urls)
  (let loop ((urls urls)
             (res ""))
    (if (null? urls)
        res
        (loop (cdr urls)
              (make-pathname res (-> (conc (car urls))
                                   (string-trim '#\/)
                                   (string-trim-right '#\/)))))))

(define (wimp-base-url path)
  (->> path
       (make-pathname (*wimp-base-url*))
       (absolute-uri)))

;; for tokens and docs, see:
;; http://stage.developer.wimpmusic.com/apps
;; https://docs.google.com/a/adellica.com/document/d/1TwYNG-bcwwwtDl1ni5TZvYIvPsh59L_GdqpNg1-aWnk
(define (wimp-login username password)
  (let ((params `((username . ,username)
                  (password . ,password)
                  (token . ,tidal-token))))
    (-> ((*wimp-query*) (wimp-base-url "login/username")
         params
         read-json)
      (select-keys '(sessionId countryCode userId)))))

(define (wimp-login-fail!)
  (abort
   (make-composite-condition
    (make-property-condition 'wimp)
    (make-property-condition 'missing-login)
    (make-property-condition 'exn
                             'message "wimp login missing"
                             'arguments '()))))

;; construct a wimp url, using the current *wimp-session-params*
(define (wimp-url name #!optional params)
  (update-uri (wimp-base-url name)
              query: (append (or (current-session-params)
                                 (wimp-login-fail!))
                             (or params '()))))

;; Perform the actual server-request, return json.
;; (call-wimp '() "tracks" 1234)
(define (call-wimp params . urls)
  (handle-exceptions exn
    ;; pick up exceptions, read server-response (json), and re-throw
    ;; error as parsed js on
    (raise (or ((condition-property-accessor 'client-error 'body) exn)
               ((condition-property-accessor 'server-error 'body) exn)
               exn))
    (let ((url (wimp-url (append-urls urls) params)))
      (print ";; => wimp: " (uri->string url))
      ((*wimp-query*) url #f read-json))))


;; construct a lambda with arguments from url parts that are symbols and a
;; query-list. urls parts that are strings are added literary. eg:
;;
;; (wimp-lambda (limit) "tracks" tid) => (lambda (tid limit #!optional params)
;; ....)
(define-syntax wimp-lambda
  (ir-macro-transformer
   (lambda (x e t)
     (let* ((params (cadr x))
            (urlspec (cddr x))
            (args (append (filter symbol? urlspec) params)))
       `(lambda (,@args #!optional params)
          (call-wimp (append (or params '())
                             (list ,@(map (lambda (x) `(cons (quote ,x) ,x)) params)))
                     ,@urlspec))))))

(define wimp-album                 (wimp-lambda ()      "albums"    aid))
(define wimp-album-tracks          (wimp-lambda ()      "albums"    aid "tracks"))
(define wimp-artist                (wimp-lambda ()      "artists"   aid))
(define wimp-artist-albums         (wimp-lambda ()      "artists"   aid "albums"))
(define wimp-artist-playlists-by   (wimp-lambda ()      "artists"   aid "playlistscreatedby"))
(define wimp-artist-playlists-with (wimp-lambda ()      "artists"   aid "playlistsincluding"))
(define wimp-artist-toptracks      (wimp-lambda ()      "artists"   aid "toptracks"))
(define wimp-playlist              (wimp-lambda ()      "playlists" uuid))
(define wimp-playlist-tracks       (wimp-lambda ()      "playlists" uuid "tracks"))
(define wimp-search-album          (wimp-lambda (query) "search"    "albums"))
(define wimp-search-artist         (wimp-lambda (query) "search"    "artists"))
(define wimp-search-playlist       (wimp-lambda (query) "search"    "playlists"))
(define wimp-search-track          (wimp-lambda (query) "search"    "tracks"))
(define wimp-track                 (wimp-lambda ()      "tracks"    tid))
(define wimp-track-streamurl       (wimp-lambda ()      "tracks"    tid "streamurl"))
(define wimp-user-playlists        (wimp-lambda ()      "users"     uid "playlists"))

;; Discovery
(define wimp-editorial-discovery-tracks (wimp-lambda () "discovery" "new" "tracks"))
(define wimp-editorial-discovery-albums (wimp-lambda () "discovery" "new" "albums"))

;; Featured
(define wimp-editorial-featured (wimp-lambda () "featured"))
(define wimp-editorial-featured-new-albums (wimp-lambda () "featured" "new" "albums"))
(define wimp-editorial-featured-new-tracks (wimp-lambda () "featured" "new" "tracks"))
(define wimp-editorial-featured-new-playlists (wimp-lambda () "featured" "new" "playlists"))

;; Moods
(define wimp-editorial-moods (wimp-lambda () "moods"))
(define wimp-editorial-moods-playlists (wimp-lambda () "moods" mood "playlists"))

;; Genres
(define wimp-genres (wimp-lambda () "genres"))
(define wimp-genres-albums (wimp-lambda () "genres" genre "albums"))
(define wimp-genres-tracks (wimp-lambda () "genres" genre "tracks"))
(define wimp-genres-playlists (wimp-lambda () "genres" genre "playlists"))

;; Favorites
(define wimp-favorites-albums (wimp-lambda () "users" uid "favorites" "albums"))
(define wimp-favorites-artists (wimp-lambda () "users" uid "favorites" "artists"))
(define wimp-favorites-playlists (wimp-lambda () "users" uid "favorites" "playlists"))
(define wimp-favorites-tracks (wimp-lambda () "users" uid "favorites" "tracks"))



;; TODO: cache this somehow. don't make requests all over the place.
(define (wimp-current-user-id)
  (alist-ref 'userId ((wimp-lambda () "sessions" id) (alist-ref 'sessionId *wimp-session-params*))))

(define (wimp-album-cover-url aid #!optional (w 100) (h 100))
  (conc "http://images.osl.wimpmusic.com/im/im?w=" w "&h=" h "&albumid=" aid))

(define (wimp-artist-image-url aid #!optional (w 100) (h 100))
  (conc "http://images.osl.wimpmusic.com/im/im?w=" w "&h=" h "&artistid=" aid))


;; TIDAL images
;; image resources are returned in an uuid-like string on the
;; following format: "0f592645-1fd6-492d-8944-d4475c688183"
;; To get an image, replace dashes with forward slashes and append
;; '/<w>x<h>.jpg' where 'w' and 'h' are height and weight.
;; As if that wasn't fun enough, different image types are stored in
;; different sizes, hence the overrides below.
;; See: http://developer.tidal.com/technical/images/
;;
;; [#178] image-id may also be other types than string, in particular 'null'.
;; When the argument is not a string, return it unchanged.
(define (tidal-image-url image-id #!optional (w 160) (h 160))
  (cond
   ((string? image-id)
	(let ((convert (lambda (str)
					 (string-translate str "-" "/"))))
	  (conc "https://resources.tidal.com/images/" (convert image-id) "/" w "x" h ".jpg")))
   (else image-id)))



(define (tidal-mood-image-url image-id)
  (tidal-image-url image-id 320 320))

(define (tidal-playlist-image-url image-id)
  (tidal-image-url image-id 160 107))

(define (tidal-genre-image-url image-id)
  (tidal-image-url image-id 220 146))

;; (wimp-login "97670550" "herrowimp")

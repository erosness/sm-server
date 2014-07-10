(use http-client uri-common medea
     clojurian-syntax
     srfi-1 srfi-18 srfi-13 files)



;; typically holds current sessionId and countryCode
(define *wimp-base-url*     (make-parameter "https://api.wimpmusic.com/v1"))
(define *wimp-query*        (make-parameter with-input-from-request))
(define *wimp-session-params* #f)

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
(define (wimp-login! username password)
  (let ((params `((username . ,username)
                  (password . ,password)
                  (clientName . "iOS_WiMP-2.5.1.no")
                  (token . "xRxdq-jJNdbCc7La"))))
    (set! *wimp-session-params* (-> ((*wimp-query*) (wimp-base-url "login/username")
                                     params
                                     read-json)
                                  (select-keys '(sessionId countryCode))))))

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
              query: (append (or *wimp-session-params*
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
    ((*wimp-query*) (wimp-url (append-urls urls) params) #f read-json)))

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

(define (wimp-album-cover-url aid #!optional (w 100) (h 100))
  (conc "http://images.osl.wimpmusic.com/im/im?w=" w "&h=" h "&albumid=" aid))

(define (wimp-artist-image-url aid #!optional (w 100) (h 100))
  (conc "http://images.osl.wimpmusic.com/im/im?w=" w "&h=" h "&artistid=" aid))


;; (wimp-login! "97670550" "herrowimp")

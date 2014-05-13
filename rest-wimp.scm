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





;; extract tid (as string) from a wimp tr uri.
(define (wimp-path->tid path)
  (match path
    (('/ "tid" tid) tid)
    (else (error "cannot find tid of path " path))))

(test-group
 "wimp-path->tid"
 (test "1234" (wimp-path->tid '(/ "tid" "1234")))
 (test-error (wimp-path->tid '(/ "wrong" "1234"))))

(define (path->suri path)
  (alist-ref 'url (wimp-track-streamurl (wimp-path->tid path))))

(define (play-command/wimp path)
  (let ((suri (path->suri path)))
    (cplay (uri-reference suri))))

(define-audio-host "wimp" play-command/wimp)

(define (track->turi track)
  (conc "tr://wimp/tid/" (alist-ref 'id track)))




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
    (artist . ,(track/album->artist-name track))
    (image  . ,(track->album-cover-uri track))))

(define (artist->search-result artist)
  `((id    . ,(alist-ref 'id artist))
    ;; This was changed to make the artist/track/album results uniform for
    ;; the client.
    ;; TODO: Properly define what a result should look like
    ;; -    (name  . ,(alist-ref 'name artist))
    ;; -    (image . ,(artist->artist-image-uri artist))))
    (title  . ,(alist-ref 'name artist))
    (cover  . ,(artist->artist-image-uri artist))
    (uri    . ,(conc "/search/wimp/artist/albums?artist=" (alist-ref 'id artist)))))

(define (album->search-result album)
  `((id     . ,(alist-ref 'id album))
    (artist . ,(track/album->artist-name album))
    (title  . ,(alist-ref 'title album))
    (cover  . ,(album->album-cover-uri album))
    (uri    . ,(conc "/search/wimp/album/tracks?album=" (alist-ref 'id album)))))


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


(define (wrap-wimp search-proc convert)
  (argumentize
   (make-wimp-search-call search-proc convert)
   'q '(limit "10") '(offset "0")))

;;==================== handlers ====================
(define-handler /search/wimp
  (lambda () `((tabs . #( ((title . "Artists") (uri . "/search/wimp/artist"))
                     ((title . "Albums")  (uri . "/search/wimp/album"))
                     ((title . "Tracks")  (uri . "/search/wimp/track")))))))

(define-handler /search/wimp/track         (wrap-wimp wimp-search-track  track->search-result))
(define-handler /search/wimp/album         (wrap-wimp wimp-search-album  album->search-result))
(define-handler /search/wimp/artist        (wrap-wimp wimp-search-artist artist->search-result))

(define-handler /search/wimp/artist/albums
  (argumentize (make-wimp-search-call wimp-artist-albums album->search-result) 'artist))

(define-handler /search/wimp/album/tracks
  (argumentize (make-wimp-search-call wimp-album-tracks track->search-result) 'album))

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
       (->> (/search/wimp/artist/albums)
            (return-wimp-uri)
            (with-request "?artist=1234")
            (irregex-search "/artists/1234")
            ((conjoin identity))))

 (test "artist/albums"
       #t
       (->> (/search/wimp/album/tracks)
            (return-wimp-uri)
            (with-request "?album=9876")
            (irregex-search "/albums/9876")
            ((conjoin identity)))))

)

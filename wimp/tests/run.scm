(use test wimp uri-common clojurian-syntax)


(test-group "append-urls"
            (test "a/1/c" (append-urls '("a" 1 "c")))
            (test "a/1/c" (append-urls '("a" 1 "//c")))
            (test "a/1/c" (append-urls '("a//" 1 "//c"))))


;;; ************************************************** testing Wimp
;;; Api

(test-begin "Wimp Rest API")

;; debug utility! return the generated url of a wimp-request instead
;; of actually doing it...
(define-syntax URL
  (syntax-rules ()
    ((_ body ...)
     (parameterize ((*wimp-query* (lambda (url . args) (uri->string url))))
       body ...))))


(test-group
 "wimp urls"
 (fluid-let ((*wimp-session-params* '((ape . "yes"))))
  (parameterize ((*wimp-base-url* "http://a.com"))

    (test "http://a.com/albums/1234/tracks?ape=yes"
          (URL (wimp-album-tracks 1234)))

    (test "http://a.com/albums/1234/tracks?ape=yes&custom=true"
          (URL (wimp-album-tracks 1234 `((custom . true))))))))

(test "login returns sessionId, userId & countryCode"
      3
      (length (wimp-login "97670550" "herrowimp")))

(test "Artist 606 is MJ"
      "Michael Jackson"
      (alist-ref 'name (wimp-artist 606)))

(test "MJ's id is 606"
      606 (->> (wimp-search-artist "Michael Jackson"  `((limit . 3)))
               (alist-ref 'items)
               ((flip vector-ref) 0)
               (alist-ref 'id)))

(test "Billie Jean is 76690"
      76690
      (->> (wimp-search-track "billie jean")
           (alist-ref 'items)
           ((flip vector-ref) 0)
           (alist-ref 'id )))

;; just test that nothing errors and that there is an http url in
;; there somewhere:
(test "streamurl exists"
      "http://"
      (substring (->> (wimp-track-streamurl 22964850)
                      (alist-ref 'url))
                 0 7))

(test "Wimp album 20889219 is MJ's"
      606
      (->> (wimp-album 20889219)
           (alist-ref 'artist)
           (alist-ref 'id)))

;; TODO: fix this
;; (wimp-search-album "Thriller")

(test 3 (->> (wimp-search-playlist "foo" `((limit . 3)))
             (alist-ref 'items)
             (vector-length)))

(test 3 (->> (wimp-artist-albums 606 '((limit . 3)))
             (alist-ref 'items)
             (vector-length)))

(test 3 (->> (wimp-artist-toptracks 606 `((limit . 3)))
             (alist-ref 'items)
             (vector-length)))

(test 1 (->> (wimp-user-playlists 1006 `((limit . 2)))
             (alist-ref 'items)
             (vector-length)))


;; search for any easy playlist, pick the first result, query it and
;; check that its uuid is the same. if we complete this loop
;; successfully, things are probably working, no?
(let ((uuid  (->> (wimp-search-playlist "easy" `((limit . 10)))
                  (alist-ref 'items)
                  ((flip vector-ref) 0)
                  (alist-ref 'uuid))))

  (test "what goes around comes around"
        uuid
        (->> (wimp-playlist uuid)
             (alist-ref 'uuid)))

  (test "playlist contains many tracks"
        10
        (->> (wimp-playlist-tracks uuid)
             (alist-ref 'items)
             (vector-length))))


(test "Madrugada has playlists"
      #t
      (>= (->> (wimp-artist-playlists-by 9000)
               (alist-ref 'totalNumberOfItems))
          1))

(test-end)
;; ************************************************** done
(test-exit)

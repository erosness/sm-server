(use wimp uri-common test clojurian-syntax restlib)

;; leaving the beuty of what is session management till later:
(wimp-login! "97670550" "herrowimp")

(define (track->turi track)
  (conc "tr://wimp/tid/" (alist-ref 'id track)))

(define (cover-uri track)
  (let ((aid (->> track
                  (alist-ref 'album)
                  (alist-ref 'id))))
    (conc "http://images.osl.wimpmusic.com/im/im?w=100&h=100&albumid=" aid)))

(define (track->artist track)
  (->> track
       (alist-ref 'artist)
       (alist-ref 'name)))

(define (track->search-result track)
  `((turi . ,(track->turi track))
    (title . ,(alist-ref 'title track))
    (artist . ,(track->artist track))
    (cover . ,(cover-uri track))))

(define (wimp-search q)
  (map track->search-result
       (->> (wimp-search-track q)
            (alist-ref 'items)
            (vector->list))))

;; extract tid (as string) from a wimp tr uri.
(define (wimp-turi-tid uri)
  (assert (equal? (uri-host uri) "wimp"))
  (assert (equal? (uri-scheme uri) 'tr))
  (assert (equal? (cadr (uri-path uri)) "tid"))
  (caddr (uri-path uri)))

(test-group
 "wimp-turi-tid"
 (test "1234" (wimp-turi-tid (uri-reference "tr://wimp/tid/1234")))
 (test-error (wimp-turi-tid (uri-reference "tr://wimp/dit/1234"))))

(define (turi->suri turi)
  (alist-ref 'url (wimp-track-streamurl (wimp-turi-tid turi))))

(define (play-command/wimp uri)
  (let ((suri (turi->suri uri)))
    (play-command/ffmpeg (uri-reference suri))))


(test
 "wimp track -> turi"
 `((turi . "tr://wimp/tid/4073803")
   (title . "Queen"))
 (track->search-result
  `((id . 4073803)
    (title . "Queen")
    (album (id . 4073801) (title . "Stoner Witch"))
    (artist (id . 14845) (name . "Melvins"))
    (duration . 187))))

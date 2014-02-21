(use wimp uri-common test clojurian-syntax restlib)

;; leaving the beuty of what is session management till later:
(print "please wait while logging in to wimp...")
(wimp-login! "97670550" "herrowimp")

(define (track->turi track)
  (conc "tr://wimp/tid/" (alist-ref 'id track)))

(define (cover-uri track)
  (let ((aid (->> track
                  (alist-ref 'album)
                  (alist-ref 'id))))
    (wimp-cover-url aid)))

(define (track->artist track)
  (->> track
       (alist-ref 'artist)
       (alist-ref 'name)))

(define (track->search-result track)
  `((turi   . ,(track->turi track))
    (title  . ,(alist-ref 'title track))
    (artist . ,(track->artist track))
    (cover  . ,(cover-uri track))))

(define (wimp-process-query-result result)
  (list->vector
   (map track->search-result
        (->> result
             (alist-ref 'items)
             (vector->list)))))

(define (wimp-search q limit offset)
  (let ((result (wimp-search-track q `((offset . ,offset)
                                       (limit  . ,limit)))))
    (make-search-result limit offset
     (alist-ref 'totalNumberOfItems result)
     (wimp-process-query-result result))))


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
    (cplay (uri-reference suri))))

(define-audio-host "wimp" play-command/wimp)
(define-handler "/search/wimp" (argumentize wimp-search 'q 'limit 'offset))

;; (play-command "tr://wimp/tid/12345")


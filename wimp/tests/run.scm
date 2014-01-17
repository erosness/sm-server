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


(parameterize ((*wimp-login-params* '((ape . "yes")))
               (*wimp-base-url* "http://a.com"))

  (test "http://a.com/albums/1234/tracks?ape=yes"
        (URL (wimp-album-track 1234)))

  (test "http://a.com/albums/1234/tracks?ape=yes&custom=true"
        (URL (wimp-album-track 1234 `((custom . true))))))

(test 2 (length (wimp-login! "97670550" "herrowimp")))

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
      (alist-ref 'id (vector-ref (alist-ref 'items (wimp-search-track "billie jean")) 0)))

;; just test that nothing errors and that there is an http url in
;; there somewhere:
(test "streamurl exists"
      "http://"
      (substring (->> (wimp-track-streamurl 22964850)
                      (alist-ref 'url))
                 0 7))

(test-end)
;; ************************************************** done

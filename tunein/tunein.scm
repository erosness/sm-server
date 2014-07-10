;;; TuneIn API wrapper
;;;
;;; typical responise is either:
;;;
;;; ((element . "outline") (type . "link") (text . "World") (URL .
;;; "http://...") (guide_id . "c57954"))
;;;
;;; or
;;;((element . "outline") (text . "Stations") (key . "stations")
;;;      (children . #(((element . "outline") (type . "audio") (text .
;;;      "JAZZABIT OSLO (Jazz)") (URL .
;;;      "http://opml.radiotime.com/Tune.ashx?id=s221815") (bitrate .
;;;      "192") (reliability . "89") (guide_id . "s221815") (subtext .
;;;      "Norwegian") (genre_id . "g11")
;;;
;;; so the element either contains a link (maps directly to our REST
;;; api), or it embeds elements. if it embeds elemenrs, we need to
;;; produce a link ourselves (and then use that link to re-query the
;;; same url and give the elements). for now, let's just dump the
;;; elements in our list too.

(use http-client medea uri-common test)


(define (tunein-uri url)

  (define uri (uri-reference url))
  ;; sane defaults
  (define uri (if (uri? uri)
                  uri
                  (uri-reference (conc "http://opml.radiotime.com/" url))))

  ;; TODO: append partnerId and serial

  (parameterize ((form-urlencoded-separator "&"))
    (uri->string
     (update-uri uri
                 query: (alist-update 'render "json"
                                      (uri-query uri))))))

(test-group
 "tunein-uri"
 (test "http://opml.radiotime.com/Browse.ashx?joe=a&render=json"
       (tunein-uri "Browse.ashx?joe=a"))

 (test "support nested tunein-uri"
       "http://x/path?render=json"
       (tunein-uri (tunein-uri "http://x/path"))))

(define (tunein-query path)
  (vector->list
   (alist-ref 'body
              (with-input-from-request (tunein-uri path)
                                       #f
                                       read-json))))

;; elisp: (put 'alist-let 'scheme-indent-function 2)
(define-syntax alist-let
  (syntax-rules ()
    ((_ alist () body ...) (begin body ...))

    ((_ alist ((var) rest ...) body ...)
     (alist-let alist ((var 'var) rest ...) body ...))

    ((_ alist ((var sym) rest ...) body ...)
     (let ((_alist alist)) ;; <-- eval alist only once
       (let ((var (alist-ref sym _alist)))
         (alist-let _alist (rest ...) body ...))))) )

(test-group
 "alist-let"
 (test "implicit symbol (same as var)" 1 (alist-let `((a . 1)) ((a)) a))
 (test "explicit symbol" 1 (alist-let `((a . 1)) ((a 'a)) a))

 (test "eval alist once"
       "x\n"
       ;; multiple bindings shouldn't eval alist more than once
       (with-output-to-string
         (lambda () (alist-let `((a . ,(print "x")))
                   ((a) (b) (c))
                 #f)))))


;; note that we have an element type "audio", and element type
;; "outline" with a type "audio". i think the outline is just one
;; level of indirection. we use turi's for outline/audio types.
;; outline/link types fit perfectly our turi/uri REST scheme.
(define (tjson->cjsons json)
  (alist-let json ((element))
    (cond ((equal? "outline" element)
           (alist-let json ((URL) (text) (children)
                            (type) (subtext) (image))
             (cond ((equal? "link" type)
                    (list ;; a list of elements
                     `((uri . ,URL)
                       (title . ,text))))
                   ((equal? "audio" type)
                    (list `((turi . ,URL)
                            (title . ,text)
                            (subtitle . ,subtext)
                            (image . ,image))))
                   (else
                    ;; no type presumably means a recursion into the
                    ;; 'children' field:
                    (if children
                        (append-map tjson->cjsons (vector->list children))
                        '())))))
          ((equal? "audio" element)
           (alist-let json ((is_direct) (url))
             (if is_direct
                 ;; no turi->suri conversion needed.
                 `((turi . ,url))
                 (error "is_direct is false" json))))
          (else (error "unknown element type" json)))))

(test-group
 "tjson->cjsons"
 (test
  "outline element, link type"
  '( ((uri . "http://popular.com")
      (title . "Most Popular")))
  (tjson->cjsons
   '((element . "outline")
     (text . "Explore Sports Talk")
     (key . "related")
     (children . #(((element . "outline")
                    (type . "link")
                    (text . "Most Popular")
                    (URL . "http://popular.com")
                    (key . "popular")))))))

 (test
  "outline element, audio type"
  `(((turi . "http://host/audio.mp3")
     (title . "title")
     (subtitle . "description")
     (image . "http://host/image.png")))
  (fluid-let ((*server-port* #f))
    (tjson->cjsons
     `((element . "outline")
       (type . "audio")
       (text . "title")
       (URL . "http://host/audio.mp3")
       (subtext . "description")
       (image . "http://host/image.png")))))

 (test
  "audio element"
  `((turi . "http://host/audio"))
  (tjson->cjsons
   '((element . "audio")
     (url . "http://host/audio")
     (is_direct . #t))))

 (tjson->cjsons
  '((element . "outline")
    (type . "text")
    (text . "No stations or shows available"))))

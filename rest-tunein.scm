(use tunein)

(import turi)

(define (tunein-turi->suri turi)
  ;; tunein happens to use the same "url" field as us. I don't think
  ;; we need to convert
  (let* ((tunein-json (first (tunein-query turi)))
         (cjson (tjson->cjsons tunein-json))
         (url    (alist-ref 'turi cjson)))
    `((url . ,url))))

(define-turi-adapter tunein-uri->turi "tunein"
  (lambda (params)
    (print "tunein adapter called!! " params)
    (and-let* ((turi (alist-ref 'id params)))
     (tunein-turi->suri turi))))

(define-handler /v1/catalog/tunein
  (lambda () `((search . #( ((title . "Stations playing Artist") (uri . ,(return-url "/catalog/tunein/search-artist")))
                       ((title . "Stations") (uri . ,(return-url "/catalog/tunein/search")))))
          (preload . #( ((title . "Browse") (uri . ,(return-url "/catalog/tunein/browse"))))))))

;; take a tunein url and make it point to ourselves
(define (rewrite-uri url)
  (conc (return-url "/catalog/tunein/browse?lnk=") (uri-encode-string url)))

;; tunein.scm's cjson's return {url :
;; "http://tunein.com/Browse?id=100" }. we need to wrap around our own
;; api for obvious reasons.
(define (rewrite-cjson cjson)
  (cond ((alist-ref 'uri cjson) =>
         (lambda (uri)
           (alist-update 'uri (rewrite-uri uri) cjson)))
        ((alist-ref 'turi cjson) =>
         (lambda (turi)
           (alist-update 'turi (tunein-uri->turi turi)
                         cjson)))
        (else (error "no uri nor turi" cjson))))

;; lnk is a parameter which, if present, points to any url that the
;; tunein api's give us. all these urls are crawled by ourselves.
(define-handler /v1/catalog/tunein/browse
  (pagize
   (argumentize (lambda (lnk)
                  (map rewrite-cjson
                       (append-map
                        tjson->cjsons
                        (tunein-query (if (string? lnk) lnk "/Browse.ashx")))))
                ;; HACK: we can't have #f as a default value ... using non-string
                '(lnk 0))))



;; TODO: refactor
(define (tunein-search q)
  (let ((encoded-query (form-urlencode `(("query" . ,q)))))
    (tunein-query (conc "/Search.ashx?" encoded-query))))

(define (tunein-search-artist q)
  (let ((encoded-query (form-urlencode `(("c" . "song,artist")
                                         ("query" . ,q)))))
    (tunein-query (conc "/Search.ashx?" encoded-query))))

(define-handler /v1/catalog/tunein/search-artist
  (pagize
   (argumentize (lambda (query)
                  (map rewrite-cjson
                       (append-map
                        tjson->cjsons (tunein-search-artist query) )))
                'q)))

(define-handler /v1/catalog/tunein/search
  (pagize
   (argumentize (lambda (query)
                  (map rewrite-cjson
                       (append-map
                        tjson->cjsons (tunein-search query) )))
                'q)))


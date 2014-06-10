;; TODO: make module?
(include "tunein.scm")

(define (tunein-turi->suri turi)
  ;; tunein happens to use the same "url" field as us. I don't think
  ;; we need to convert
  (let ((tunein-json (first (tunein-query (uri-decode-string turi)))))
    (or (alist-ref 'is_direct tunein-json) (error "missing is_direct" tunein-json))
    tunein-json))

(define-turi-adapter tunein-uri->turi "tunein" tunein-turi->suri)

(define-handler /v1/catalog/tunein
  (lambda () `((tabs . #( ((title . "Browse") (uri . ,(return-url "/catalog/tunein/browse"))))))))

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
           (alist-update 'turi (tunein-uri->turi (uri-encode-string turi))
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


(uri-decode-string "http%3A%2F%2Fopml.radiotime.com%2FBrowse.ashx%3Fid%3Dc424725%26filter%3Dl111&q=&limit=25&offset=0")

(pp (tunein-query "http://opml.radiotime.com/Browse.ashx?id=c424725&filter=l111"))

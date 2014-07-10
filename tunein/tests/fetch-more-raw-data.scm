;; read from test-data-urls.scm and store their raw content as files.
;; do this manually once in a blue moon. the raw content is commited
;; for easier test-runs.
(use tunein http-client uri-common test)

;; we only want to inspect non-direct links. get a list of them.
(define urls
  (filter-map (lambda (obj) (and (not (alist-ref 'is_direct obj))
                            (alist-ref 'url obj)))
              (include "./test-data-urls.scm")))

;; (pp (sort urls string>))

;; do a `curl` on url and store raw body content in a file.
(define (store url)
  (handle-exceptions
      e (begin (pp (condition->list e)) #f)
      (with-input-from-request url #f (cut read-string (* 1024 50)))))

(for-each (lambda (url)
            (print ";; " url)
            (write `("" . ,(store url)))
            (print))
          urls)

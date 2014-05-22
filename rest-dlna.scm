
(use dlna)

(import rest)

(define *devices* (ssdp-search))

;; dlna searches may return containers and tracks. containers are
;; useful for browsing. tracks should contain turis available for
;; playback.
(define (track? talist)
  (and (pair? talist)
       (eq? (car talist) 'track)))

(define ((make-dlna-search-call search) q)

  ;; not yet supported
  (define limit 10)
  (define offset 0)

  (let ((result (filter track? (search (*devices*) q))))
    (make-search-result limit offset
                        (length result)
                        (map cdr result))))

(define (%dlnas p) (argumentize (make-dlna-search-call p) 'q))

;; note: we don't need an audio host because turi's should be http
;; from the UPnP server directly.
(define-handler /v1/catalog/dlna/artist (%dlnas dlna-search/artist))
(define-handler /v1/catalog/dlna/album  (%dlnas dlna-search/album))
(define-handler /v1/catalog/dlna/track  (%dlnas dlna-search/track))

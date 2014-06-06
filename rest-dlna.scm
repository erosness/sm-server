
(use dlna restlib spiffy srfi-18
     looper)

(define *devices* (lambda () '()))

;; this is a bit hacky. we keep searching, but we should really have
;; implemented the multicast listener interface for incoming
;; server-up/server-down messages. this way, we'll only find new
;; servers every 60 seconds and puts extra pressure on the network.
(begin
  (handle-exceptions e #f (thread-terminate! *dlna-search-thread*))
  (define *dlna-search-thread*
    (thread-start!
     (make-thread
      (loop (lambda ()
              ;; let's request a search and wait for replies for 1
              ;; minute.
              (define temp-searcher (ssdp-search 60))
              ;; let's not modify *devices* immediately because it'd
              ;; temporarily say that there are 0 devices before
              ;; anybody replies. let's wait for some replies:
              (thread-sleep! 10)
              ;; now make these new discoveries available outside:
              (set! *devices* temp-searcher)
              ;; let it slide for a while before we search again
              (thread-sleep! 50)))
      "*dlna-search-thread*"))))

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

(define-handler /v1/catalog/dlna
  (lambda () `((tabs . #( ((title . "Browse") (uri . ,(return-url "/catalog/dlna/browse"))))))))

(define (talist->mblist talists)
  (map
   (lambda (item)
     (let ((type (car item))
           (alst (cdr item)))
       ;; TODO: assuming talists have the same format as our
       ;; media-browser rest apis. this may be a bad thing.
       (case type
         ((container)
          `((uri . ,(conc "/catalog/dlna/browse?id=" (alist-ref 'id alst)))
            ,@(alist-delete 'id alst)))
         ((track) (alist-delete 'id alst))
         (else (error "invalid talist" item)))))
   talists))

(test-group
 "rest-dlna talist->mblist"

 (test
  `(((uri . "/catalog/dlna/browse?id=0") (title . "foo"))
    ((uri . "uri") (title . "bar")))
  (talist->mblist `((container (id . "0") (title . "foo"))
                    (track (uri . "uri")  (title . "bar") (id . "gone!"))))))

(define-handler /v1/catalog/dlna/browse
  ;; TODO: note that we query the server for all search-results, and
  ;; just drop off the ones our cube-clients don't want. there's
  ;; probably a way to limit the search-query in this way for the
  ;; DLNA-server too.
  (pagize
   (lambda ()
     (append-map
      (lambda (devc)
        (talist->mblist
         (didl->talist
          (browse-query devc
                        (or (and (current-request)
                                 (current-query-param 'id))
                            "0")))))
      (content-directories (*devices*))))))

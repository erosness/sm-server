
(use dlna restlib spiffy srfi-18
     srfi-13 ;; string-hash
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



;; ==================== glossary ====================
;;
;; id - id param in turi (t2s&type=dlna&id=...)
;; bid - browse-id, string used to id DLNA containers
;; service - url to a DLNA content-dir service
;; sid - hash of service url (for turi shortness)

;; compute a sid based on service-string. just to shorten long urls
;; down to some "hash" value. we use this hash to pick the original
;; url back. chance of collision should be small! specially
;; considering there are normally only one or two possible originals.
(define (service->sid service)
  (conc (string-hash service 99999)))

;; return original service-string from sid or #f
(define (sid->service sid services)
  (find (lambda (s) (equal? (service->sid s) sid))
        services))

;; turi id string (uri-encoded) <====> service & browse-id. a service
;; is a url, and the the browse-id is the container id of the folder
;; to browse.
(define (service&bid->id service #!optional (bid "0"))
  ;; add preceeding b for "browse", in case we need an "s" for
  ;; "search" later.
  (uri-encode-string (conc "b" (service->sid service) "." bid)))

(define (id->service&bid id)
  (let* ((id (uri-decode-string id))
         (idx (string-index id #\.)))

    (unless (equal? (substring id 0 1) "b")
      (error "invalid id" id))

    (cons (substring id 1 idx) ;; remove "b" prefix
          (substring id (add1 idx)))))

(define (id->service id #!optional (devs (content-directories (*devices*))))
  (sid->service (car (id->service&bid id)) devs))

(define (id->bid id)
  (cdr (id->service&bid id)))

(test-group
 "id->service and friends"

 (test "b5543.0"   (service&bid->id "foo"))
 (test "b5543.%3A" (service&bid->id "foo" ":"))

 (test "0" (id->bid "b5543.0"))
 (test ":" (id->bid "b5543.%3A"))

 (test "foo" (id->service "b5543.XXX" '("foo")))

 ;; the ultimate challenge. crazy strings back and forth.
 (let* ((services '("*&:?://dawdfaf.!@$#%^&(*^^_&^>.#$#%+__+:" "b"))
        (service (car services))
        (bid "(*&DAWDAD^$#%R.>:AW\"D?><+_+_-_+")
        ;; final conversion:
        (id (service&bid->id service bid)))
   ;; (print "id is " id)
   (test bid     (id->bid id))
   (test service (id->service      id services))))

;; compute a proper return-url for browsing.
(define (service-browse-uri service #!optional (bid "0"))
  (return-url "/catalog/dlna/browse?id=" (service&bid->id service bid)))


;; list all services in a turi-like fashion.
(define (rest-dlna-services)
  (map (lambda (service)
         `((uri . ,(service-browse-uri service))
           (title . ,"TODO: friendlyname")
           (subtitle . ,(return-url service))))
       (content-directories (*devices*))))

(define (talist->mblist talists service)
  (map
   (lambda (item)
     (let ((type (car item))
           (alst (cdr item)))
       ;; TODO: assuming talists have the same format as our
       ;; media-browser rest apis. this may be a bad thing.
       (case type
         ((container)
          `((uri . ,(service-browse-uri service (alist-ref 'id alst)))
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

;; browse-query for `service`
(define (dlna-query/turi service bid)
  (talist->mblist
   (didl->talist
    (browse-query service bid))
   service))

;; for descriptive errors and easy repl access to sids.
(define (services&sids)
  (map (lambda (service) (cons service (service->sid service)))
      (content-directories (*devices*))))

(define-handler /v1/catalog/dlna/browse
  ;; TODO: note that we query the server for all search-results, and
  ;; just drop off the ones our cube-clients don't want. there's
  ;; probably a way to limit the search-query in this way for the
  ;; DLNA-server too.
  (pagize
   (argumentize (lambda (id)
                  (cond ((number? id) ;; <-- hack for argumentize (no #f default)
                         (rest-dlna-services)) ;; <-- list all DLNA servers
                        (else ;; browse a particular server (we have id param)
                         (let* ((service (or (id->service id)
                                             (error "sid not found" id
                                                    (services&sids))))
                                (bid (id->bid id)))
                           (dlna-query/turi service bid)))))
                '(id 0))))

;;; Testing REST APIs of cube-server
;;;
;;; - tests pagination of hardcoded catalogs providers (see tabs definition)
;;; - tests media-browser items (turi / uri item)
;;; - tests response of /player/current (paused, pos, duration etc)
;;;
;;; TODO: test notifications (use echo field and wait for that echo on
;;; udp).
;;;
;;; TODO: don't assume player/catalog is the same (split tests?)

(use pefat)
(use intarweb uri-common test http-client medea matchable)

;; alist helpers
(include "incubator.scm")
(import incubator)

;; current-player etc
(include "client-lib.scm")

;; setting some less surprising defaults
(form-urlencoded-separator "&;")


(match (command-line-arguments)
  ((remote)
   (let ((uri (uri-reference remote)))
     (current-player (inet-address (uri-host uri) (uri-port uri)))))
  (else (error "usage: <http://<host>:<port>> # maestro's address")))

(define (query path #!optional post)
  (let ((uri (current-base-url path)))
    (print ";; querying " uri)
    (values
     (with-input-from-request uri
                              (and post (json->string post))
                              read-json))))

;; generate a turi based on current player. TODO: get this from the
;; /catalog/ interface instead!
(define (tone-turi tone)
  (let ((uri (uri-reference (current-base-url ""))))
    (uri->string
     (update-uri uri
                 scheme: 'tr
                 port: (uri-port uri)
                 path: (append (uri-path uri) (list "t2s"))
                 query: `((type . "tone")
                          (id . ,tone))))))

(define (test-current turi response)
  (test "turi unchanged"       turi (alist-ref 'turi response))
  (test "not paused "          #f   (alist-ref 'paused response))
  (test "position early (<1s)" #t   (<  (alist-ref 'pos response) 1))
  ;; assuming we never play clips shorter than a second
  (test "duration (<1s)"       #t   (>= (alist-ref 'duration response) 1)))

(test-begin "player")

(test-group
 "simple /player/current"

 (let ((turi (tone-turi 400)))
   (test-current turi (query "/player/current" `((turi . ,turi))))
   (test-current turi (query "/player/current"))))

(test-group
 "simple volume"

 (test "volume setter" `((value . 10)) (query "/player/volume" `((value . 10))))
 (test "volume setter" `((value . 13)) (query "/player/volume" `((value . 13))))
 (test "volume getter" `((value . 13)) (query "/player/volume")))

(test-end "player")

;; check that tabs is json where each element has a uri field.
(define (validate-tabs tabs)
  (map (lambda (js)
         (let ((uri   (alist-ref 'uri js))
               (title (alist-ref 'title js)))
           (test #t (string? uri))
           (test #t (string-prefix? "/catalog/" uri))
           (test #t (string? title))
           (cons uri title)))
       (vector->list  (or (alist-ref 'search tabs)
                          (alist-ref 'preload tabs)))))


;; quickly test that the "recursive" browsing of media-browsing urls
;; point back to the catalog. they don't technically have to do that,
;; but it's an easy test when they all follow this convention.
(define (test-mb-uri url)
  (test "mb uri starts with /catalog" "/catalog" (substring url 0 8)))

;; only one of uri or turi, not both, not neither.
(define (valid-mb? item)

  ;; can the uri be parsed (with scheme, host and everything?)
  (define (good-uri? x) (string? (alist-ref x item)))

  (= 1 (bitwise-xor (if (good-uri?  'uri) 1 0)
                    (if (good-uri? 'turi) 1 0))))

(define (validate-mb item)
  (cond ((alist-ref 'uri item) => test-mb-uri))
  (test "valid media-browser item?" #t (valid-mb? item)))

(test-group
 "internal valid-mb?"
 (test #f (valid-mb? `((uri . #f) (turi . #f))))
 (test #t (valid-mb? `((uri . #f) (turi . "/catalog"))))
 (test #t (valid-mb? `((uri . "/catalog") (turi . #f))))
 (test #f (valid-mb? `((uri . "/catalog") (turi . "/catalog")))))

;; pick out the list of items in the response.
(define (items x)
  (vector->list (alist-ref 'items x)))

;; with three elements of three offsets, there overlapping elements of
;; each
(define (validate-pagination* items-1 items+0 items+1)
  (test "pagination ( *=* * )" (list-ref items-1 2) (list-ref items+0 1))
  (test "pagination ( * *=* )" (list-ref items+0 1) (list-ref items+1 0)))

(validate-pagination* '( ((a . 1)) ((b . 2)) ((c . 3)) )
                      '( ((b . 2)) ((c . 3)) ((d . 4)) )
                      '( ((c . 3)) ((d . 4)) ((e . 5)) ))

(define (validate-pagination url #!optional (offset 1))

  (define (rl offs) (conc url "&limit=3&offset=" offs))
  (validate-pagination* (items (query (rl (+ offset -1))))
                        (items (query (rl (+ offset  0))))
                        (items (query (rl (+ offset +1))))))

(define tabs
  (append
   ;;(validate-tabs (query "/catalog/wimp"))
   (validate-tabs (query "/catalog/tunein"))
   (validate-tabs (query "/catalog/dlna"))
   ))


(test-begin "media browser apis")
(for-each
 (lambda (tab)
   (let ((url (conc (car tab) "?q=ab")))
     (for-each validate-mb (items (query url)))
     (validate-pagination url)))
 tabs)
(test-end "media browser apis")

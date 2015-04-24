(include "dlna-sql.scm")

(import dlna-sql)
(use spiffy uri-common intarweb restlib regex test
     clojurian-syntax srfi-14)

;; Static file server
; assume a static file server is running, serving files from
; *web-root* on port *static-server-port*.

;; Port where static file server is listening
(define *static-server-port* 5057)

(cond-expand
 ((not arm)
  ;; Location in filesystem where USB stick is mounted
  (define *web-root* "/tmp")
  ;; Path to check for database file
  (set-db! "/tmp/mediadb/files.db"))
 (else
  ;; TODO: what paths should be use?
  (define *web-root* "/data")
  (set-db! "/data/files.db")
  ))

;; URL encoding

;; We're serving up arbitrary audio files here, so their names can be anything.
;; Escape everything except [A-Za-z0-9.]
(define char-set:a-z (ucs-range->char-set (char->integer #\a)
                                          (char->integer #\z)))

(define char-set:A-Z (ucs-range->char-set (char->integer #\A)
                                          (char->integer #\Z)))
(define (url-encode s)
  (uri-encode-string
   s
   (char-set-complement
    (char-set-union char-set:a-z
                    char-set:A-Z
                    char-set:digit
                    (char-set #\.)))))

(test-group
 "url-encoding"
 (test "ascii letters and digits are not encoded"
       "foobar123" (url-encode "foobar123"))
 (test "non a-z A-Z 0-9 chars are encoded"
  "%C3%A6%C3%B8%C3%A53a%CC%8A" (url-encode "æøå3å"))
 (test "" (url-encode "")))

;;;

;; TODO: create global debug logger
(define (dbg . args)
  (print "DEBUG: " (apply conc args)))

;;;

(define (transform-duration duration-str)
  ;; INPUT:   Duration string (like "H:M:S.MS")
  ;; RETURNS: Duration in seconds or #f

  (and-let* (;; Split on separators -> list
         (fields (string-split-fields "[:.]" duration-str #:infix))
         ;; Assert we got at least 3 fields
         ((<= 3 (length fields)))
         ;; convert to number
         (fields (map string->number fields)))

    ;; Multiply hours, minutes and seconds to obtain
    ;; duration in seconds
    (fold + 0 (map * '(3600 60 1) fields))))

(test-group
 "transform-duration"
 (test 142 (transform-duration "0:02:22.933"))
 ;; (+ (* 3600 1) (* 60 34) 12) => 5652
 (test (+ (* 3600 1) (* 60 34) 12)
       (transform-duration "01:34:12"))
 (test #f (transform-duration "invalid input")))

;;;


(define (filename->turi webroot filename)
  (and-let* ((filename)
             ;; Check that 'filename' is under webroot
             ((string-prefix? webroot filename))
             (filename (string-drop filename (+ 1 (string-length webroot)))))
    (url-encode  filename)))

(test-group
 "filename->turi"
 (test
  "some.mp3"
  (filename->turi "/tmp/foo" "/tmp/foo/some.mp3"))
 (test
  "name%20with%20spaces.mp3"
  (filename->turi "/tmp/foo" "/tmp/foo/name with spaces.mp3"))
 (test
  #f
  (filename->turi "/tmp/foo" "/outside/webroot")))

;;;

(define (container? res)
  (cond ((alist-ref 'type res) => (lambda (x) (string-prefix? "container." x)))
        (else #f)))

(define (track? res)
  (cond ((alist-ref 'type res) => (lambda (x) (equal? "item.audioItem.musicTrack" x)))
        (else #f)))

(define (response-container res)
  (let ((get (cut alist-ref <> res)))
    `((title .    ,(get 'name))
      (uri . ,(return-url "/catalog/usb/browse?id=" (get 'id))))))

(define (response-track res)
  (let* ((get (cut alist-ref <> res))
         (host (uri-host (request-uri (current-request))))
         (turi (filename->turi *web-root* (get 'filename))))

    (or turi
        (print (conc "ERROR: file '" (get 'filename) "' is not served by server (serving: '" *web-root* "')")))

    `((title . ,(get 'title))
      (subtitle . ,(get 'artist))
      (type . "usb")
      (duration . ,(transform-duration (get 'duration)))
      (turi . ,(conc "tr://" host ":" *static-server-port* "/" turi)))))

(define (transform-result res)
  (cond ((container? res) (response-container res))
        ((track? res) (response-track res))
        (else (print "unknown row: " res))))

(define (transform-results res)
  (map transform-result res))

;;;

(define ((%make-results proc) q)
  (let* ((ret (or (proc q)
                  ;; No database found, usb stick not present
                  (response-unavailable))))
    (transform-results ret)))

(define-handler /v1/catalog/usb
  (lambda () `((search .  #( ((title . "Search") (uri . ,(return-url "/catalog/usb/search")))))
          (preload . #( ((title . "Browse") (uri . ,(return-url "/catalog/usb/browse"))))))))

(define-handler /v1/catalog/usb/browse
  (pagize (argumentize (%make-results browse) '(id "1"))))

(define-handler /v1/catalog/usb/search
  (pagize (argumentize (%make-results search) 'q)))

(include "dlna-sql.scm")

(import dlna-sql rest)
(use spiffy uri-common)

(define *mount-point* "/tmp")
(define *server-port* 1034)

(set-db! "/tmp/files.db")

(define (urlencode s)
  (uri-encode-string
   s
   (char-set-delete
    (char-set-complement char-set:uri-unreserved)
    #\/ #\:)))

; /mount/usb/foo/bar.mp3 -> http://<host>:<port>/foo/bar.mp3
(define (tid-transform host tid)
  (let ((base-url (conc "http://" host ":" *server-port*)))
    (assert (string-prefix? *mount-point* tid)
            (conc "files outside " *mount-point* " not allowed"))

    (urlencode
     (conc base-url (string-drop tid (string-length *mount-point*))))))

(define (result-transform result)
  (let ((host (uri-host (request-uri (current-request)))))
   (map (lambda (res) (alist-update
                  'tid
                  (tid-transform host (alist-ref 'tid res))
                  res)) result)))

(define ((%make-results proc) q limit offset)
  (let ((result (result-transform (proc q limit offset))))
    (make-search-result limit offset (length result) result)))

(define-handler /v1/browse/usb (argumentize (%make-results browse) 'q '(limit "10") '(offset "0")))
(define-handler /v1/catalog/usb (argumentize (%make-results search) 'q '(limit "10") '(offset "0")))

(define (start-static-file-server)
  (root-path *mount-point*)
  (vhost-map `((".*" . ,(lambda (cont) (cont)))))
  (start-server port: *server-port*))

;; (define t (thread-start! (lambda () (start-static-file-server))))

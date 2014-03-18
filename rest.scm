(module rest (*uris*
              set-handler!
              define-handler
              define-audio-host
              play-command
              wrap-changes)

(import chicken scheme data-structures player)
(use srfi-69 ports test uri-common medea
     broadcast)

;; ==================== handler ====================
(define *uris* (make-hash-table))

(define (set-handler! url thunk)
  (assert (string? url))
  (hash-table-set! *uris* url thunk))

(define-syntax define-handler
  (syntax-rules ()
    ((define-handler path body ...)
     (begin
       (define path body ...)
       (set-handler! (symbol->string 'path) path)))))

;; ==================== audio hosts ====================

;; provide an API for audio hosts / providers to plug into.
(define *audio-hosts* `())
(define (define-audio-host host handler)
  (set! *audio-hosts*
        (alist-update host handler *audio-hosts* equal?)))

(define (play-command turi)
  ;; uri may be #f if uri-ref can't parse turi
  (let ((uri (if (uri? turi) turi (uri-reference turi))))
    (case (and uri (uri-scheme uri))
      ((tr) ((or
              ;; pick the procedure registered for host:
              (alist-ref (uri-host uri) *audio-hosts* equal?)
              ;; error if none found:
              (lambda _ (error "unknown audio host" (uri-host uri))))
             ;; call audio-host procedure with one arg:
             uri))
      ;; default to cplay with any other scheme (file://, http:// etc)
      (else (cplay (or uri (error "illegal uri" turi)))))))


;;==================== rest combinators ====================

(define ((wrap-changes path proc) #!rest args)
  (let* ((response (apply proc args))
         (json (with-output-to-string (lambda () (write-json response)))))
    (udp-broadcast (make-udp-message path json))
    response))


(test-group
 "play-command"

 (test '("cplay" "file:///filename") (play-command "file:///filename"))
 (test '("cplay" "http://domain/file.mp3") (play-command "http://domain/file.mp3"))

 (test '("cplay" "filename") (play-command "filename"))
 (test-error (play-command "i l l e g a l")))
)

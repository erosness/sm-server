(use fmt test uri-common)

(include "process-cli.scm")
(import process-cli)


;; create shell string for launching `cplay` player daemon. launch it
;; with play!.
(define (cplay source #!optional (format #f))
  (let ((lformat (if format (list "-f" format) '()))
        (lsource (list (cond ((uri-reference? source) (uri->string source))
                             (else source)))))
    (append '("cplay") lformat lsource)))

(test-group
 "cplay"
 (test '("cplay" "filename") (cplay "filename"))
 (test '("cplay" "filename") (cplay (uri-reference "filename")))
 (test '("cplay" "-f" "alsa" "file") (cplay "file" "alsa")))



;; spawn command, killing the previous one if it's running
;; TODO: support on-exit callback
(define play!
  (let ((current #f))
    (lambda (lcommand)
      (if current (current #:quit))
      (set! current (process-cli (car lcommand)
                                 (cdr lcommand)
                                 (lambda () (print "*** song finshed"))))
      current)))

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

(test-group
 "play-command"

 (test "cplay \"file:///filename\"" (play-command "file:///filename"))
 (test "cplay \"http://domain/file.mp3\"" (play-command "http://domain/file.mp3"))

 (test "cplay \"filename\"" (play-command "filename"))
 (test-error (play-command "i l l e g a l")))

;; (define stop (play! (cplay (uri-reference "tone://sine/440"))))
;; (read-string #f stop)
;; (stop)

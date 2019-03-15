#!/bin/csi


;; Begin general nano partitions

(use posix nanomsg matchable test string-utils)

(define (make-nano-socket addr)
  (let ((nnsock (nn-socket 'pair)))
    (nn-connect nnsock addr)
    nnsock))

;; Record type to handle communication with gstplay.
(define-record-type gp (%make-gst response handlers socket gst-mutex read-thread)
  gp?
  (response get-response set-response)
  (handlers get-handlers set-handler)
  (socket get-socket make-socket)
  (gst-mutex get-mutex)
  (read-thread get-thread))

(define-record-printer gp
  (lambda (rec out)
    (fprintf out "gp handlers: ~S responses:"
                  (if (get-handlers rec) "Has handler" "No handler"))))

(define (make-gst addr)
  (let* ((socket (make-nano-socket addr))
        (mtx (make-mutex))
        (response #f))
    (%make-gst #f #f socket (make-mutex) #f)))

(define (parse-response resp)
  (print "Response from gstplay: " resp))

(define (nn-send* rec msg)
  (let ((sock (get-socket rec))
        (mtx  (get-mutex rec) ))
    (dynamic-wind
      (lambda () (mutex-lock! mtx))
      (lambda () (nn-send sock msg))
      (lambda () (mutex-unlock! mtx)))))

;; Add blocking thread to fetch meesages over nanomsg connection.
;; Currently only strict requst-respnse messages. TODO: out-of-band
;; push messages for tag update and status change (typically end-of-track)
(define (make-nano-read-thread socket response)
  (let ((response (get-response rec))
        (sock (get-socket rec)))
    (thread-start!
      (lambda ()
        (print "Make thread A")
        (-->
          (lambda () (print "Run thread"))
          (loop/interval 1)
          (loop)
          ((flip make-thread) "NanoReadThread"))))))

(define (gst-request rec msg #!optional (parser #f))
  (let* ((sock (get-socket rec))
        (cmd-string* (symbol-list->string msg))
        (cmd-string (string-append cmd-string* "\n")))
          (nn-send* rec cmd-string)
          (let ((response (nn-recv sock)))
            (if parser
              (parser response)
              response))))

(define (get-msg rec)
  (let ((sock (get-socket rec)))
    (nn-recv* sock nn/dontwait)))

;; end general nano part
;; parsers

(define (parse-cplay-pos-response resp)
  (match (string-split resp " #\x0a;#\x00;")
    (("ok" pos duration)
      (let ((pos-number (string->number pos))
            (duration-number (string->number duration)))
        (values (if pos-number pos-number 0)
                (if duration-number duration-number 0))))
    (else (values 0 0))))

(test-group
 "parse cplay pos"
 (test "parse cplay pos - success"
       '(23.2341 42.43)
       (receive (parse-cplay-pos-response "ok 23.2341 42.43")))

  (test "parse cplay pos - success with tail"
       '(93.2341 45.23)
       (receive (parse-cplay-pos-response "ok 93.2341 45.23\n#\x00;")))

 (test "parse cplay pos - success with huge number and tail"
      '(1298129893.2341 45.23)
      (receive (parse-cplay-pos-response "ok 1298129893.2341 45.23\n#\x00;")))

 (test "parse cplay pos - success, report 0 for number garbage"
       '(0 45.23)
       (receive (parse-cplay-pos-response "ok per 45.23\n#\x00;")))

 (test "parse cplay pos - failure"
       '(0 0)
       (receive (parse-cplay-pos-response "some garbage 1234"))))


    (define (symbol-list->string cmd-list)
      (let* ((token-raw (car cmd-list))
            (token-str (if (symbol? token-raw)
           	                (symbol->string token-raw)
                            token-raw)))
              (if (not (null? (cdr cmd-list)))
                (string-append token-str " " (symbol-list->string (cdr cmd-list)))
                token-str)))

(define (parse-cplay-paused?-response resp)
 (and-let* ((value (string-split resp " #\x0a;#\x00;"))
            ((equal? (length value) 2))
            (value (cadr value))
            ((or (equal? value "false") (equal? value "true"))))
   (equal? value "true")))

(test-group
 "parse cplay paused?"
 (test "parse cplay pos - success, paused"
       '(#t)
       (receive (parse-cplay-paused?-response "ok true")))

 (test "parse cplay pos - success, not paused"
       '(#f)
       (receive (parse-cplay-paused?-response "ok false")))

 (test "parse cplay pos - fail, not paused"
       '(#f)
       (receive (parse-cplay-paused?-response "ok false per"))))


;; end parsers
;; player part


(define (prepause-spotify)
  (with-input-from-pipe "spotifyctl 7879 pause" void)
  (thread-sleep! 0.3))

;; Control operations
(define (player-pause)           (gst-request gstplayer `(pause)))
(define (player-unpause)         (prepause-spotify) (gst-request gstplayer `(unpause)))
(define (player-spotify-unpause) (gst-request gstplayer  `(unpause)))
(define (player-paused?)         (gst-request gstplayer `(paused?) parse-cplay-paused?-response))
(define (player-pos)             (gst-request gstplayer `(pos) parse-cplay-pos-response))
;;(define (player-pos)             (gst-request gstplayer `(pos)))
(define (player-duration)        (receive (pos duration)
                                   (gst-request gstplayer `(pos) parse-cplay-pos-response)
                                   duration))


(define (player-seek seek)       (prepause-spotify) (gst-request gstplayer `(seek ,seek)))
(define (player-quit)            (gst-request gstplayer  `(quit)))
;; cplay running and not paused:
(define (playing?)   (and (not (eq? #f (gst-request gstplayer `(pos))))
                          (not (player-paused?))))
(define (player-nexttrack?) (gst-request gstplayer  `(nexttrack?)))
(define (play! pcommand on-exit on-next)
;;  (setup-nexttrack-callback on-next)
  (prepause-spotify)
  (gst-request gstplayer pcommand))

(define gstplayer (make-gst "ipc:///data/nanomessage/playcmd.pair"))

;; Test part
;;(define gst-socket (make-nano "ipc:///data/nanomessage/playcmd.pair"))


(print (player-pos))
(sleep 1)
(print (player-pos))



(sleep 1)
(print ": " (get-msg gstplayer))
(sleep 1)
(print "Play:" (play! `("play" "http://listen.181fm.com/181-70s_128k.mp3?noPreRoll=true") (lambda () (print "At A")) (lambda () (print "At B")) ))

(print "Paused?: "(player-paused?))
(sleep 1)
(print "Paused?: "(player-paused?))
(print ": " (get-msg gstplayer))
(sleep 1)
(print (player-pos))
(print ": " (get-msg gstplayer))
(player-pause)
(sleep 1)
(print "Paused?: "(player-paused?))
(print "Duration?: "(player-duration))
(sleep 1)
(print ": " (get-msg gstplayer))
(print (player-pos))
(player-unpause)
(sleep 1)
(print "Paused?: "(player-paused?))
(print ": " (get-msg gstplayer))
(sleep 1)
(print ": " (get-msg gstplayer))
;;  (print (player-pos))
(sleep 1)
(print (player-pos))
(print ": " (get-msg gstplayer))
(sleep 1)
(print (player-pos))

(exit)

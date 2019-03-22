;; Nano interface module. Establish interface according to IXION standard (see
;; TS1001 for details). Consists of one request-response connection based
;; on the pair protocol and one subscribe channel for metadata etc.
(module nanoif ( make-nano-if
                 nano-if-request)

(import extras chicken scheme srfi-1)
(use nanomsg clojurian-syntax looper srfi-18 data-structures)

(define (symbol-list->string cmd-list)
  (let* ((token-raw (car cmd-list))
        (token-str (if (symbol? token-raw)
                        (symbol->string token-raw)
                        token-raw)))
          (if (not (null? (cdr cmd-list)))
            (string-append token-str " " (symbol-list->string (cdr cmd-list)))
            token-str)))

(define (make-req-socket addr)
  (let ((nnsock (nn-socket 'pair)))
    (nn-connect nnsock addr)
    nnsock))

  (define (make-pub-socket addr)
    (let ((nnsock (nn-socket 'sub)))
      (nn-connect nnsock addr)
      (nn-subscribe nnsock "") ;; Subscribe to eveything.
      nnsock))

;; Record type to handle communication with gstplay.
(define-record-type nano-if (%make-nano-if req-socket
                                           pub-socket
                                           req-mutex
                                           pubhandlers
                                           response)
  nano-if?
  (req-socket get-req)
  (pub-socket get-pub)
  (req-mutex get-mutex)
  (handlers get-handlers set-handler)
  (response get-response set-response))

(define-record-printer nano-if
  (lambda (rec out)
    (fprintf out "nano-if handlers: ~S responses:"
                  (if (get-handlers rec) "Has handler" "No handler"))))

(define (make-nano-if req-addr pub-addr)
  (print "Begin make-nano-if")
  (%make-nano-if
    (make-req-socket req-addr)
    (make-pub-socket pub-addr)
    (make-mutex)
    #f
    #f))

;; Add blocking thread to fetch meesages over nanomsg connection.
;; Currently only strict requst-respnse messages. TODO: out-of-band
;; push messages for tag update and status change (typically end-of-track)
(define (make-nano-pull-thread)
;; Read all messages in a blocking loop. Sort messages as response and
;; push messages based on grammar.
  (define (make-pull-socket)
     (let ((pull-sock (nn-socket 'sub)))
        (nn-connect pull-sock  "ipc:///data/nanomessage/test.pub")
        pull-sock))

  (define (read-nanomsg)
    (let* ((pull-socket (make-pull-socket)))
;;      (nano-if-request rec `(pos))
      (print "Before... in " (thread-name (current-thread)))
      (let ((msg (nn-recv pull-socket)))
        (print "XX" pull-socket " - " msg))))

  (thread-start!
    (->>
      read-nanomsg
      (loop/interval 0.01)
      (loop)
      ((flip make-thread) "NanoReadThread"))))


(define (nano-if-request rec msg #!optional (parser #f))
  (let ((sock (get-req rec))
        (mtx  (get-mutex rec) ))
    (dynamic-wind
      (lambda () (mutex-lock! mtx))
      (lambda ()
        (let* ((cmd-string* (symbol-list->string msg))
              (cmd-string (string-append cmd-string* "\n")))
          (nn-send sock cmd-string)
          (let ((response (nn-recv sock)))
            (if parser
              (parser response)
              response))))
      (lambda () (mutex-unlock! mtx)))))

) ;; module nanoif ends

;; Nano interface module. Establish interface according to IXION standard (see
;; TS1001 for details). Consists of one request-response connection based
;; on the pair protocol and one subscribe channel for metadata etc.
(module nanoif  (make-nano-if
                 nano-if-request)

(import extras chicken scheme srfi-1)
(use nanomsg clojurian-syntax looper srfi-18 data-structures medea)

;; Helper function used to prepare message payload
(define (symbol-list->string cmd-list)
  (let* ((token-raw (car cmd-list))
        (token-str (if (symbol? token-raw)
                        (symbol->string token-raw)
                        token-raw)))
          (if (not (null? (cdr cmd-list)))
            (string-append token-str " " (symbol-list->string (cdr cmd-list)))
            token-str)))

;; Main structure, contains one request interface and one subscribe interface
(define-record-type nano-if (%make-nano-if req-if pub-if)
  nano-if?
  (req-if get-req)
  (pub-if get-pub))

(define (make-nano-if req-addr pub-addr)
  (print "Begin make-nano-if")
  (let ((rec (%make-nano-if
              (make-req-if req-addr)
              (make-sub-if pub-addr))))
      (print "Did etablish both interfaces")
      rec))

;; ==== Request interface starts here
;; Create suitable socket
(define (make-req-socket addr)
  (let ((socket (nn-socket 'pair)))
    (nn-connect socket addr)
    socket ))

;; Record type to handle communication with gstplay.
(define-record-type req-if (%make-req-if req-socket
                                           req-mutex
                                           response)
  req-if?
  (req-socket get-req-socket)
  (req-mutex get-mutex)
  (response get-response set-response))

  (define (make-req-if req-addr)
    (let ((rec (%make-req-if
              (make-req-socket req-addr)
              (make-mutex)
              #f)))
      rec))

;; ==== Subscribe interface starts here
;; Create suitable socket
(define (make-sub-socket addr)
  (let ((nnsock (nn-socket 'sub)))
    (nn-connect nnsock addr)
    (nn-subscribe nnsock "") ;; Subscribe to eveything.
    nnsock))


  ;; Record type to handle communication with gstplay.
(define-record-type sub-if (%make-sub-if sub-socket handlers sub-thread)
  sub-if?
  (pub-socket get-sub-socket)
  (handlers get-handlers set-handler)
  (sub-thread get-sub-thread set-sub-thread))

(define-record-printer sub-if
  (lambda (rec out)
    (fprintf out "nano-if handlers: ~S responses:"
                  (if (get-handlers rec) "Has handler" "No handler"))))

(define (make-sub-if pub-addr)
  (let* ((socket (make-sub-socket pub-addr))
        (rec (%make-sub-if
            socket
            #f
            #f)))
    (set-sub-thread rec (make-sub-thread socket #f))
    rec))

;; Blocking thread to fetch meesages over nanomsg connection.
;; Currently only strict requst-respnse messages. TODO: out-of-band
;; push messages for tag update and status change (typically end-of-track)
(define (make-sub-thread socket handlers)
;; Read all messages in a blocking loop. Sort messages as response and
;; push messages based on grammar.
  (define (read-nanomsg)
    (let* ((pull-socket socket)
           (msg (nn-recv pull-socket)))
      (print "Received push message: " msg " = " (read-json msg))))

  (thread-start!
    (->>
      read-nanomsg
      (loop/interval 0.1)
      (loop)
      ((flip make-thread) "NanoReadThread"))))

(define (nano-if-request* sock msg)
  (nn-send sock msg)
  (nn-recv sock))

(define (nano-if-request/timeout sock msg)
  (let ((nano-thread
          (make-thread
            (lambda () (nano-if-request* sock msg))
            "Nano-work-thread")))
            (thread-start! nano-thread)
            (let ((result (thread-join! nano-thread 4 #f)))
              (print "Got result from thread:" result)
              result)))

(define (nano-if-request rec msg #!optional (parser #f))
  (let* ((req-rec (get-req rec))
        (sock (get-req-socket req-rec))
        (mtx  (get-mutex req-rec)))
    (dynamic-wind
      (lambda () (mutex-lock! mtx))
      (lambda ()
        (let* ((cmd-string* (symbol-list->string msg))
              (cmd-string (string-append cmd-string* "\n")))
          (let ((response (nano-if-request/timeout sock cmd-string)))
            (if parser
              (parser response)
              response))))
      (lambda () (mutex-unlock! mtx)))))

) ;; module nanoif ends

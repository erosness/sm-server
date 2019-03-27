;; Nano interface module. Establish interface according to IXION standard (see
;; TS1001 for details). Consists of one request-response connection based
;; on the pair protocol and one subscribe channel for metadata etc.
(module nanoif  (make-nano-if
                 make-nano-half-if
                 nano-if-request
                 set-handler)

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
(define-record-type nano-if (%make-nano-if req-if sub-if)
  nano-if?
  (req-if get-req)
  (sub-if get-sub))

(define (make-nano-half-if req-addr)
  (print "Begin make-nano-if")
  (let ((rec (%make-nano-if
              (make-req-if req-addr)
              #f)))
      rec))

(define (make-nano-if req-addr pub-addr)
  (print "Begin make-nano-if")
  (let ((rec (%make-nano-if
              (make-req-if req-addr)
              (make-sub-if pub-addr))))
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
  (sub-socket get-sub-socket)
  (handlers get-handler %set-handler)
  (sub-thread get-sub-thread set-sub-thread))

;; Print for subscribe record, for debug
(define-record-printer sub-if
  (lambda (rec out)
    (fprintf out "nano-if handlers: ~S responses:"
                  (if (get-handler rec) "Has handler" "No handler"))))

;; Default push handler, just prints the incoming datum.
(define (default-push-handler msg)
  (print "Incoming message:" msg))

;; Create the subscriber part of the interface
(define (make-sub-if pub-addr)
  (if pub-addr
  (let* ((socket (make-sub-socket pub-addr))
        (rec (%make-sub-if
            socket
            default-push-handler
            #f)))
    (set-sub-thread rec (make-sub-thread rec))
    rec)
    #f))

;; Exported procedure to set the push message handler.
(define (set-handler rec handler)
  (let* ((sub-if (get-sub rec)))
    (%set-handler sub-if handler)))

;; Blocking thread to fetch meesages over nanomsg push-connection.
(define (make-sub-thread sub-if)

;; Read next JSON object as a message and decode
  (define (read-nanomsg)
    (let* ((pull-socket (get-sub-socket sub-if))
           (msg (nn-recv pull-socket))
           (obj (read-json msg))
           (push-handler (get-handler sub-if)))
      (push-handler obj)))

;; create the thread itself.
  (thread-start!
    (->>
      read-nanomsg
      (loop/interval 0.1)
      (loop)
      ((flip make-thread) "NanoReadThread"))))

;; Exchange the messages (request - response)
(define (nano-if-request* sock msg)
  (nn-send sock msg)
  (nn-recv sock))

;; Message exchange with timeout (...not yet properly installed)
(define (nano-if-request/timeout sock msg)
  (let ((nano-thread
          (make-thread
            (lambda () (nano-if-request* sock msg))
            "Nano-work-thread")))
            (thread-start! nano-thread)
            (let ((result (thread-join! nano-thread 4 #f)))
              result)))

;; Do the request operation.
(define (nano-if-request rec msg #!optional (parser #f))
  (let* ((req-rec (get-req rec))
        (sock (get-req-socket req-rec))
        (mtx  (get-mutex req-rec)))
    (dynamic-wind
      (lambda () (mutex-lock! mtx))
      (lambda ()
        (let* ((cmd-string* (symbol-list->string msg))
              (cmd-string (string-append cmd-string* "\n\x00")))
          (let ((response (nano-if-request/timeout sock cmd-string)))
            (if parser
              (parser response)
              response))))
      (lambda () (mutex-unlock! mtx)))))

) ;; module nanoif ends

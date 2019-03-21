;; Nano interface module. Establish interface according to IXION standard (see
;; TS1001 for details). Consists of one request-response connection based
;; on the pair protocol and one subscribe channel for metadata etc.
(module nanoif * ;; (make-nano-if
;;                nano-if-request
;;                )

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

(define (make-nano-socket addr)
  (let ((nnsock (nn-socket 'pair)))
    (nn-connect nnsock addr)
    nnsock))

;; Record type to handle communication with gstplay.
(define-record-type nano-if (%make-nano-if socket req-mutex handlers response)
  nano-if?
  (socket get-socket make-socket)
  (req-mutex get-mutex)
  (handlers get-handlers set-handler)
  (response get-response set-response))

(define-record-printer nano-if
  (lambda (rec out)
    (fprintf out "nano-if handlers: ~S responses:"
                  (if (get-handlers rec) "Has handler" "No handler"))))

(define (make-nano-if addr)
  (print "Begin make-nano-if")
  (%make-nano-if (make-nano-socket addr) (make-mutex) #f #f))

(define (nn-send* rec msg)
  (let ((sock (get-socket rec)))
    (nn-send sock msg)))

;; Add blocking thread to fetch meesages over nanomsg connection.
;; Currently only strict requst-respnse messages. TODO: out-of-band
;; push messages for tag update and status change (typically end-of-track)
(define (make-nano-pull-thread)
;; Read all messages in a blocking loop. Sort messages as response and
;; push messages based on grammar.
  (define (make-pull-socket)
     (let ((pull-sock (nn-socket 'sub)))
        (nn-connect pull-sock  "ipc:///data/nanomessage/test.pub")
        (nn-subscribe pull-sock "")
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
  (let ((sock (get-socket rec))
        (mtx  (get-mutex rec) ))
    (dynamic-wind
      (lambda () (mutex-lock! mtx))
      (lambda ()
        (let* ((cmd-string* (symbol-list->string msg))
              (cmd-string (string-append cmd-string* "\n")))
          (nn-send* rec cmd-string)
          (let ((response (nn-recv sock)))
            (if parser
              (parser response)
              response))))
      (lambda () (mutex-unlock! mtx)))))

(define (get-msg rec)
  (let ((sock (get-socket rec)))
;;    (nn-recv* sock nn/dontwait)))
    (nn-recv sock)))
    ) ;; module nanoif ends

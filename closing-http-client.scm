(module closing-http-client (with-input-from-request*)
(import chicken scheme)
(use http-client medea tcp intarweb uri-common)

;; like with-input-from-request but force connection keep-alive to off
;; (it's on by default on HTTP/1.1). since each thread gets its own
;; connection pool of keep-alive connections, spawing many threads
;; where all of which create new lingering connection, we soon run
;; into "too many open files".
;;
;; what we do here to fix this is explicitly adding a "Connection:
;; close" header (which makes http-client close connection immediately).
(define (with-input-from-request* req writer reader)
  (let ((req (cond ((request? req) req)
                   ((uri? req) (make-request uri: req))
                   ((string? req) (make-request uri: (uri-reference req))))))
    (with-input-from-request
     (update-request
      req
      headers: (replace-header-contents 'connection
                                        '(#(close ()))
                                        (request-headers req)))
     writer reader)))


)

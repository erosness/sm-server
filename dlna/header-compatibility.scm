;;; ============================== portability
;;; testing against upnp server app on android
;;; (https://play.google.com/store/apps/details?id=com.bubblesoft.android.bubbleupnp),
;;; we run into header problems.
;;;
;;; default-header-unparser uses dquotes and escapes " etc. we need to
;;; remove these on content-type and *always* add quotes on soapaction (meh -
;;; stupid UPnP servers). withous quotes on soapaction, my android upnp
;;; mediaserver fails.

;;; the http standard allows quoting but this upnp server
;;; (unsurprisingly) doesn't gives a 415 "Unsupported Media Type" when
;;; the content-type header value is wrapped in quotes.


;; eval thunk with raw unparsers on soapaction and content-type. this
;; allows explicit quotes on soapaction and no quotes on content-type.
(define (with-soap-unparsers thunk)

  (define (raw-unparser lst)
    (list (vector-ref (car lst) 0)))

  (define (force-quotes-unparser lst)
    (let ((str (vector-ref (car lst) 0)))
      (if (string-contains str "\"")
          (error "soapaction header has quotes" str))
      (list (conc #\" str #\"))))

  ;; intarweb specifics:
  (parameterize ((header-unparsers `((content-type . ,raw-unparser)
                                     (soapaction   . ,force-quotes-unparser)
                                     ,@(header-unparsers))))
    (thunk)))


(test-group
 "specific soap headers"

 (define (serialize-request-headers req)
   (let ((strport (open-output-string)))
     (write-request (update-request req port: strport))
     (get-output-string strport)))

 (test
  "serialize-request-headers"
  "GET * HTTP/1.1\r
Content-Type: a b c\r
Soapaction: \"x\"\r
\r
"
  (with-soap-unparsers
   (lambda () (serialize-request-headers (make-request headers: (headers `((content-type "a b c")
                                                                      (soapaction "x")))))))))

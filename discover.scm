;;; Send discover announcements to the discovery server
;;;

(use http-client medea uri-common irregex files ports data-structures fmt
     (only intarweb request? make-request response-port response-headers header-value)
     test clojurian-syntax looper srfi-18 socket)


(define writer
  (json->string '(( name . "Tes2 Maestro")(icon . 5))))

(define (discover-announce*)
  (let*
;;    ((uri "http://10.0.0.11:5055/v1/player/icon")
    ((uri (absolute-uri "http://discover.ixionaudio.com/reg.php"))
     (reader read-string))
       (receive (data uri-res response)
;;	 (uri
         (with-input-from-request uri writer read-json)
	 (print uri-res)
	 (print response)
         data)))

(define discover-check-response
  (lambda (thunk)
    (let* ((data (thunk))       
;;	  (response-status (alist-ref 'status data)))
	  (response-status data))
      (print "Erik:" data " - " response-status)
;;      (if
;;        (and
;;	  response-status
;;	  (eqv? response-status 'ok))
;;        data
      ;;        #f))))
      data)))

(define (discover-announce)
  (thread-start!
    (make-thread
     (->> (lambda () (discover-announce*))
	  (discover-check-response)
          (loop/exceptions (lambda (e) (pp `(error: ,(current-thread)
						     ,(condition->list e)))))
;;					#f)
	  (loop/interval 1)
	  (loop/count 2)
          (loop))
      "discover-thread")))

(discover-announce))





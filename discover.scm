;;; Send discover announcements to the discovery server
;;;

(use http-client medea uri-common irregex files ports clojurian-syntax looper srfi-18)

(define mac
  (let
    ((mac1  (irregex-extract  '(= 2 hex-digit)
       (read-string #f (open-input-file "/sys/fsl_otp/HW_OCOTP_MAC1"))))
     (mac0  (irregex-extract  '(= 2 hex-digit)
       (read-string #f (open-input-file "/sys/fsl_otp/HW_OCOTP_MAC0")))))
    (conc
     (car mac1) ":"
     (cadr mac1) ":"
     (car mac0) ":"
     (cadr mac0) ":"
     (caddr mac0) ":"
     (cadddr mac0))))
  
(define lan-ip
(irregex-match-substring
  (irregex-search
    '(: "inet " (=> ip (or "192" "10")
                (= 1 "." (** 1 3 numeric))
	        (= 1 "." (** 1 3 numeric))
	        (= 1 "." (** 1 3 numeric))))
    (with-input-from-pipe "ip -o -f inet a|grep wlan0" read-string))
   'ip))

(define writer
  (json->string `(( uid . ,mac)(ip . ,lan-ip))))

(define (discover-announce*)
  (let*
    ((uri (absolute-uri "http://discover.ixionaudio.com/register-maestro.php"))
     (reader read-string))
       (receive (data uri-res response)
         (with-input-from-request uri writer read-json)
         data)))

(define discover-check-response
  (lambda (thunk)
    (let* ((data (thunk))       
	  (response-status (alist-ref 'status data)))
      (if
        (and
	  response-status
	  (eqv? response-status 'Ok))
        data
        #f))))

(define (discover-announce)
  (thread-start!
    (make-thread
          (->> (lambda () (discover-announce*))
          (loop/exceptions (lambda (e) (pp `(error: ,(current-thread)
                                                   ,(condition->list e)))
					#f))
	  (discover-check-response)
	  (loop/interval 4)
	  (loop/count 3)
          (loop))
     "discover-thread")))

(discover-announce)












(use spiffy intarweb uri-common
     srfi-69 restlib test
     clojurian-syntax)


(include "rest-wimp.scm")
(include "player.scm")

(define play
  (let ((cache #f))
    (lambda ()
      (let ((json (current-json)))
        (if json
            (let ((song (alist-ref 'turi json)))
              (begin (play! (play-command song))
                     (print "playing " song)
                     (set! cache song)
                     `((status . "ok"))))
            cache)))))


(define (tone-search q)
  (->> (map (lambda (hz)
              (let ((hz (* hz 100)))
                `((title . ,(conc hz " hertz"))
                  (duration . 1)
                  (turi . ,(conc "tr://tone/" hz)))))
            (map add1 (iota 100)))
       (filter (lambda (alist) (irregex-search q (alist-ref 'turi alist))))))

(test `("tr://tone/1000" "tr://tone/10000")
      (map (cut alist-ref 'turi <>) (tone-search "1000")))


;; (hash-table->alist *uris*)
(define *uris* (alist->hash-table `(("/search/tone" . ,(pagize (argumentize 'q tone-search)))
                                    ("/search/wimp" . ,(pagize (argumentize 'q wimp-search)))
                                    ("/play" .   ,(lambda a (apply play a))))))

;; (find-accessor "/search")
(define (find-accessor uri #!optional (uris *uris*))
  (hash-table-ref/default uris uri #f))

(define (json-handler)
  (let ((uri (uri->string (make-uri path: (uri-path (request-uri (current-request)))))))
    (let ((handler (find-accessor uri)))
      (if handler
          (handler)
          `((error       . ,(conc "not found: " uri))
            (valid-urls  . ,(list->vector (hash-table-keys *uris*))))))))

(define handler (->> (lambda () (json-handler))
                     (wrap-json)
                     (wrap-errors)))

(vhost-map `((".*" . ,(lambda (continue) (handler)))))

;; for your repl pleasure:
;; (define thread (thread-start! (lambda () (start-server port: 5055))))

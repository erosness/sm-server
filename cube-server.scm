(use spiffy intarweb uri-common
     srfi-69 restlib test
     clojurian-syntax)


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
                  (turi . ,(conc "tone://sine/" hz)))))
            (map add1 (iota 100)))
       (filter (lambda (alist) (irregex-search q (alist-ref 'turi alist))))))

(test `("tone://sine/1000" "tone://sine/10000")
      (map (cut alist-ref 'turi <>) (tone-search "1000")))

(define (search)
  (or
   (and-let* ((query (alist-ref 'q (uri-query (request-uri (current-request))))))
     (paginate (tone-search query) (current-limit) (current-offset)))
   (error "no search (try /search?q=1)")))


;; (hash-table->alist *uris*)
(define *uris* (alist->hash-table `(("/search" . ,(lambda a (apply search a)))
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

(define handler (wrap-errors (wrap-json (lambda _ (json-handler)))))

(vhost-map `((".*" . ,(lambda _ (handler)))))

;; for your repl pleasure:
;; (define thread (thread-start! (lambda () (start-server port: 5055))))

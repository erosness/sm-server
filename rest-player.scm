(module rest-player ()

(import chicken scheme data-structures)

(import rest player)

(define (parse-cplay-pos-response resp)
  (and-let* ((l (drop (string-split resp) 1))
         (pos (string->number (car l)))
         (total (string->number (cadr l))))
    `((pos . ,pos)
      (total . ,total))))

(test "parse cplay pos - success"
      '((pos . 23.2341)
        (total . 45.23))
      (parse-cplay-response "ok 23.2341 45.23"))

(test "parse cplay pos - failure"
      #f (parse-cplay-pos-response "some garbage 1234"))

(define (parse-cplay-paused?-response resp)
  (and-let* ((value (string-split resp))
             ((equal? (length value) 2))
             (value (cadr value))
             ((or (equal? value "false") (equal? value "true"))))
    `((value . ,(equal? value "true")))))

(test-group
 "parse-cplay-paused?"
 (test "truthy" `((value . #t)) (parse-cplay-paused?-response "ok true"))
 (test "falsy"  `((value . #f)) (parse-cplay-paused?-response "ok false"))
 (test "bad input" #f (parse-cplay-paused?-response "ok asdf"))
 (test "more bad input" #f (parse-cplay-paused?-response "foo"))
 (test "empty input" #f (parse-cplay-paused?-response "")))

(define-handler /player/pause
  (wrap-changes "/player/pause"
                (lambda ()
                  (and-let* ((res (player-paused?))
                             (parsed (parse-cplay-paused?-response res)))
                    (if (current-json)
                        (let* ((current-state (alist-ref 'value parsed))
                               (value (alist-ref 'value (current-json))))
                          (if (not (equal? current-state value))
                              (if value
                                  (player-pause)
                                  (player-unpause)))
                          `((value . ,value)))
                        parsed)))))

(define-handler /player/pos
  (wrap-changes "/player/pos"
                (lambda ()
                  (and-let* ((res (player-pos))
                             (parsed (parse-cplay-pos-response res)))
                    (if (current-json)
                        (let* ((value (alist-ref 'pos (current-json))))
                          ;; TODO: player-seek should return the
                          ;; current position after seek instead of
                          ;; just returning 'ok', faking it for now
                          (player-seek value)
                          `((pos . ,value)
                            (total . ,(alist-ref 'total parsed))))
                        parsed))))))

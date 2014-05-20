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

(define-handler /pause
  (lambda () (player-pause)))

(define-handler /unpause
  (lambda () (player-unpause)))

(define-handler /pos
  (wrap-changes "/pos"
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

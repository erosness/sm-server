(module rest-player ()

(import chicken scheme data-structures)

(import rest player)
(use test restlib clojurian-syntax)

(define-handler /player/pause
  (wrap-changes "/player/pause"
                (lambda ()
                  (and-let* ((parsed (player-paused?)))
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
                  (and-let* ((parsed (player-pos)))
                    (if (current-json)
                        (let* ((value (alist-ref 'pos (current-json))))
                          ;; TODO: player-seek should return the
                          ;; current position after seek instead of
                          ;; just returning 'ok', faking it for now
                          (player-seek value)
                          `((pos . ,value)
                            (total . ,(alist-ref 'total parsed))))
                        parsed))))))


;; ==================== seek position hearbeat ====================
(import broadcast)
(use looper multicast medea)


;; do this on every player hearbeat interval
(define (player-thread-iteration)
  (cond ((player-pos) =>
         (lambda (pos)
           (->> pos
                (json->string)
                (change-message "/v1/player/pos")
                (udp-multicast))))))

(define player-seek-thread
  (thread-start!
   (make-thread
    (->> (lambda ()
           (player-thread-iteration)
           (thread-sleep! 1))
         (loop))
    "player-seek-thread")))

;; (thread-terminate! player-thread)
;; (thread-state player-thread)

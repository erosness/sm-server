(module rest-player ()

(import chicken scheme data-structures)

(import rest player playqueue)
(use test restlib clojurian-syntax ports
     srfi-18 extras
     medea broadcast multicast incubator)

(define *pq* (make-pq
              '( ((id . "400") (turi . "tr://localhost:5060/t2s?type=tone&id=400"))
                 ((id . "800") (turi . "tr://localhost:5060/t2s?type=tone&id=800"))
                 ((id . "999") (turi . "tr://localhost:5060/t2s?type=tone&id=999")))))

(define ((change-callback path) oldval newval)
  (udp-multicast (change-message path newval *server-port*)))

(pq-add-current-change-listener
 *pq* (change-callback "/v1/player/current"))



;; alist of position, duration, paused etc (or '() if nothing is
;; playing)
(define (player-pos-info)
  (if (player-pos) ;; <- active cplay?
      (alist-merge `((pos .      ,(player-pos))
                     (duration . ,(player-duration))
                     (paused .   ,(player-paused?))))
      '()))


;; Manipulate current track.
;; POST: Looks for three keys; turi, paused, pos.
;; If turi is present adds this item to pq and starts playing.
;; If paused is present, toggles pause state
;; If pos is present, seek to that position
;; Returns: new value of current
;; GET: returns value of current with updated pos.
(define-handler /v1/player/current
  (lambda ()
    (if (current-json)
        (let* ((json-request (current-json))
               (existing (pq-ref *pq* json-request))
               (current (pq-current *pq*)))

          ;; Change track?
          (if (or (alist-ref 'turi json-request)
                  (alist-ref 'id json-request))
              (let ((queue-item (or existing (pq-add *pq* json-request))))
                (pq-play *pq* queue-item #f)
                (set! current queue-item)))

          ;; Change pos?
          (and-let* ((pos (assoc 'pos json-request)))
            (player-seek (cdr pos)))

          ;; Change paused?
          (and-let* ((pause (assoc 'paused json-request)))
            (if (cdr pause) (player-pause) (player-unpause)))

          ;; Set and NOTIFY new current value
          (let ((new-current (alist-merge current (player-pos-info))))
            (pq-current-set! *pq* new-current)
            new-current))
        ;else
        (alist-merge (pq-current *pq*) (player-pos-info)))))

;; Adds an item to the back of the playqueue
;; Returns: the passed in item with a unique id added
(define-handler /v1/player/pq/add
  (lambda () (let ((json (current-json)))
          (cond ((vector? json)
                 ;; add a list of items
                 (->> (vector->list json)
                      (pq-add-list *pq*)
                      (list->vector)))
                ;; add a single item
                (else (pq-add *pq* json))))))


;; Removes and item referenced by id from the playqueue
;; Does nothing if id is not found in playqueue
(define-handler /v1/player/pq/del
  (lambda () (let* ((id (alist-ref 'id (current-json))))
          (pq-del *pq* id)
          `((status . "ok")))))


;; Removes every item from the playqueue and stops the player
(define-handler /v1/player/pq/clear
  (lambda () (pq-clear *pq*)
     (player-quit)
     `((status . "ok"))))


;; Returns the playqueue
(define-handler /v1/player/pq (lambda () (list->vector (pq-list *pq*))))

(define-handler /v1/player/next
  (lambda () (let ((nx (pq-next *pq*)))
          (pq-play-next *pq*)
          nx)))

(define-handler /v1/player/prev
  (lambda () (let ((nx (pq-prev *pq*)))
          (pq-play-prev *pq*)
          nx)))


;; (/player/pq/next)
;; (/player/pq/prev)

;; ==================== seek position hearbeat ====================
(import broadcast)
(use looper multicast medea)


;; do this on every player hearbeat interval
(define (player-thread-iteration)
  (if (playing?) ;; running and not paused?
      (udp-multicast
       (change-message "/v1/player/pos"
                       (player-pos-info)
                       *server-port*))))

(define player-seek-thread
  (thread-start!
   (make-thread
    (->> (lambda ()
           (thread-sleep! 1)
           (player-thread-iteration))
         (loop/exceptions (lambda (e) (pp `(error: ,(current-thread)
                                              ,(condition->list e))) #t))
         (loop))
    "player-seek-thread")))

;; (thread-terminate! player-thread)
;; (thread-state player-thread)

)

(module rest-pq ()

(import chicken scheme data-structures srfi-1 ports)

(use restlib medea clojurian-syntax)

(import broadcast
        rest player
        playqueue
        multicast
        incubator)

(define *pq* (make-pq
              '( ((id . "400") (turi . "tr://localhost:5060/t2s?type=tone&id=400"))
                 ((id . "800") (turi . "tr://localhost:5060/t2s?type=tone&id=800"))
                 ((id . "999") (turi . "tr://localhost:5060/t2s?type=tone&id=999")))))

(define ((change-callback path) oldval newval)
  (let ((json (with-output-to-string (lambda () (write-json newval)))))
    (udp-multicast (change-message path json))))

(pq-add-current-change-listener
 *pq* (change-callback "/v1/player/current"))

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
        (let* ((item (current-json))
               (existing (pq-ref *pq* item))
               (current (pq-current *pq*)))

          ;; Change track?
          (if (or (alist-ref 'turi item)
                  (alist-ref 'id item))
              (let ((item (or existing (pq-add *pq* item))))
                (pq-play *pq* item #f)
                (set! current item)))

          ;; Change pos?
          (and-let* ((pos (assoc 'pos item)))
            (player-seek (cdr pos)))

          ;; Change paused?
          (and-let* ((pause (assoc 'paused item)))
            (if (cdr pause) (player-pause) (player-unpause)))

          ;; Set and NOTIFY new current value
          (let ((new-current (alist-merge current
                                          (player-pos)
                                          (player-paused?))))
            (pq-current-set! *pq* new-current)
            new-current))
        ;else
        (alist-merge (pq-current *pq*)
                     (player-pos)
                     (player-paused?)))))

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

;; Returns the playqueue
(define-handler /v1/player/pq (lambda () (list->vector (pq-list *pq*))))
)

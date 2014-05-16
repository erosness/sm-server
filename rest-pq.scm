(module rest-pq ()

(import chicken scheme data-structures srfi-1 ports)

(use restlib medea)

(import broadcast
        rest player
        playqueue
        multicast)

(define *pq* (make-pq
              '( ((id . "400") (turi . "tr://localhost:5060/t2s?type=tone&id=400"))
                 ((id . "800") (turi . "tr://localhost:5060/t2s?type=tone&id=800"))
                 ((id . "999") (turi . "tr://localhost:5060/t2s?type=tone&id=999")))))

(define ((change-callback path) oldval newval)
  (let ((json (with-output-to-string (lambda () (write-json newval)))))
    (udp-multicast (change-message path json))))

(pq-add-current-change-listener
 *pq* (change-callback "/pq/play"))

;; Adds an item to the back of the playqueue and starts it.
;; Returns: the passed in item with a unique id added
(define-handler /pq/play
  (wrap-changes "/pq/play"
                (lambda ()
                  (let* ((item (current-json))
                         (existing (pq-ref *pq* item))
                         (item (or existing (pq-add *pq* item))))
                    (print "playing " item)
                    (pq-play *pq* item)
                    item))))

;; Adds an item to the back of the playqueue
;; Returns: the passed in item with a unique id added
(define-handler /pq/add
  (wrap-changes "/pq/add"
                (lambda () (pq-add *pq* (current-json)))))


;; Removes and item referenced by id from the playqueue
;; Does nothing if id is not found in playqueue
(define-handler /pq/del
  (wrap-changes "/pq/del"
                (lambda () (let* ((id (alist-ref 'id (current-json))))
                        (pq-del *pq* id)
                        `((status . "ok"))))))


;; Removes every item from the playqueue and stops the player
(define-handler /pq/clear
  (pq-clear *pq*)
  (player-quit)
  `((status . "ok")))

(define-handler /pq/play/next
  (let ((nx (pq-next *pq*)))
    (pq-play-next *pq*)
    nx))

(define-handler /pq/play/prev
  (let ((nx (pq-prev *pq*)))
    (pq-play-prev *pq*)
    nx))

;; (/pq/play/next)
;; (/pq/play/prev)

;; Returns the playqueue
(define-handler /pq (lambda () (list->vector (pq-list *pq*))))
)

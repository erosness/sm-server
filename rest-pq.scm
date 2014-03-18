(module rest-pq ()

(import chicken scheme data-structures srfi-1)

(use restlib)

(import broadcast concurrent-utils rest player playqueue)
;; (include "playqueue.scm")

(define *pq* (make-pq))

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
  (wrap-changes "/pq/clear"
                (lambda () (pq-clear *pq*)
                   (player-quit)
                   `((status . "ok")))))

(define-handler /pq/play/next
  (wrap-changes "/pq/play/next"
                (lambda () (let ((nx (pq-next *pq*)))
                        (pq-play-next *pq*)
                        nx))))

(define-handler /pq/play/prev
  (wrap-changes "/pq/play/prev"
                (lambda () (let ((nx (pq-prev *pq*)))
                        (pq-play-prev *pq*)
                        nx))))

;; (/pq/play/next)
;; (/pq/play/prev)

;; Returns the playqueue
(define-handler /pq (lambda () (list->vector (pq-list *pq*))))
)

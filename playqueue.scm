(module playqueue (pq-list
                   pq-prev
                   pq-play-prev
                   pq-next
                   pq-play-next
                   pq-clear
                   pq-del
                   pq-ref
                   pq-add
                   pq-add-list
                   pq-current
                   pq-current-set!
                   pq-loop
                   pq-loop-set!
                   pq-play
                   make-pq
                   pq-add-current-change-listener)

(import chicken scheme data-structures srfi-1)
(use srfi-18 uri-common test uuid extras)

(import concurrent-utils player)

(include "state-var.scm")

;; (include "concurrent-utils.scm")

;; the playqueue is the abstractions clients use to manipulate whats
;; currently playing. the contents of a playqueue is a playqueue item
;; (pq-item); which is an alist with the key 'turi present. when an
;; item is inserted into the queue it gets an unique id assigned to it
;; under the key 'id.
;;
;; the playqueue supports the following operations
;;
;;  - add: adds an element to the back of the queue
;;  - del: remove and item by id from the queue
;;  - clear: remove all items from the queue
;;  - play: play an item referenced by id




(define (add-back l item)
  (append l (list item)))

(test-group
 "add-back"
 (test "add to empty list" '(1)     (add-back '()    1))
 (test "add to non-empty"  '(1 2 3) (add-back '(1 2) 3)))

(define (assert-pq-item item)
  (assert (alist-ref 'turi item)))

(test-group
 "assert-pq-item"
 (test-assert "valid item" (assert-pq-item '((turi . "tr://wimp/play"))))
 (test-error  "wrong key"  (assert-pq-item `((foo . "tr://wimp/play")))))


;; ==================== pq record ====================
(define-record-type pq (%make-pq list mutex current loop)
  pq?
  (list pq-list pq-list-set!)
  (mutex pq-mutex)
  (current %pq-current)
  (loop pq-loop pq-loop-set!))

(define-record-printer (pq x port)
  (display (conc "#<pq " (length (pq-list x)) " current: "
                 (state-value (%pq-current x)) ">") port))

(define (make-pq #!optional (lst '()) (current #f) (loop #f))
  (%make-pq lst (make-mutex) (make-state current) loop))

(define (pq-current pq)
  (state-value (%pq-current pq)))

(define (pq-current-set! pq v)
  (state-change (%pq-current pq) v))

(define (pq-add-current-change-listener pq listener)
  (state-add-listener (%pq-current pq) listener))

;; ****************************
;; public api

;; The id of the track currently playing, an item with id *pq-current*
;; should always be available in the playqueue
;; (define *pq-current* #f)


(define (pq-ref* pq item)
  (let ((id (alist-ref 'id item)))
    (find (lambda (elem) (equal? (alist-ref 'id elem) id))
          (pq-list pq))))

;; chance of collision is 2 / (expt 62 4). in the future we could use
;; pq to check that the id doesn't already exists.
;;
;; (map make-unique-id (iota 40))
(define make-unique-id
  (let ((alphabet (string->list "abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")))
    (lambda (pq)
      (list->string
       (list-tabulate 4 (lambda (idx) (list-ref alphabet (random (length alphabet)))))))))

(define (pq-add* pq item)
  (assert-pq-item item)
  (and (alist-ref 'id item)
       (error "item already has id field" item))

  (let ((item (alist-cons 'id (make-unique-id pq) item)))
    (pq-list-set! pq (add-back (pq-list pq) item))
    item))

;; this is tricky because we have a list of alists. our incoming json
;; will use vectors, but lets be very explicit about things.
(define (pq-add-list* pq items)
  (map (cut pq-add* pq <>) items))

(define (pq-del* pq item)
  (or (pq-ref* pq item) (error "cannot find" item))
  (pq-list-set! pq (delete item (pq-list pq))))

(define (pq-clear* pq)
  (pq-list-set! pq '())
  (pq-current-set! pq #f))

(define (pq-next/lst* pq lst)
  (and
   (pq-current pq)
   (and-let* ((id (alist-ref 'id (pq-current pq)))
          (tail (find-tail
                 (lambda (x) (equal? id (alist-ref 'id x)))
                 lst)))
     ;; return the next element of the first match
     (and (pair? (cdr tail)) (cadr tail)))))

;; play either the obvious next song, or if we're at the last song,
;; play first iff looping is enabled
(define (pq-next* pq #!optional (force-loop #f))
  (or (pq-next/lst* pq (pq-list pq))

      (and-let* (((or (pq-loop pq) force-loop
                      (not (pq-current pq))))
                 ((not (null? (pq-list pq)))))
        (car (pq-list pq)))))

(define (pq-prev* pq) (pq-next/lst* pq (reverse (pq-list pq))))

(define (pq-play* pq item #!optional (update-current #t))
  (let* ((item (or (pq-ref* pq item) (error "not found in pq" item)))
         (track (alist-ref 'turi item)))
    (play! (play-command track)
           (lambda ()
             ;; try to play next song, if anything goes wrong, print
             ;; and exit. it's important we check for errors here,
             ;; otherwise we get abandoned mutexes.
             ;;
             ;; the tricky part here is that this will catch errors
             ;; inside pq-play* too because pq-play-next will call it.
             ;; we only want to catch those errors, we do not want to
             ;; catch the errors that were caused by a direct curl
             ;; /v1/player/current because those errors are nicely
             ;; propegated back to the REST response. the errors in
             ;; this callback, however, will crash the server:
             (handle-exceptions e (pp `(play-next warning ,(condition->list e)))
                                (pq-play-next pq))))
    (print "playing " track)
    (if update-current
        (pq-current-set! pq item))))

;; Play next song
(define (pq-play-next* pq #!optional (force-loop #f))
  (or (and-let* ((next (pq-next* pq force-loop)))
        (pq-play* pq next))
      (begin
        (pq-current-set! pq #f)
        (player-quit))))


;; Play previous song
;; - if current song has played for more than 2 seconds, seek to start
;; - if this is the first song, seek to start
(define (pq-play-prev* pq)
  (or (and-let* ((prev (pq-prev* pq))
                 (pos (player-pos)))
        (if (< 2.0 pos)
            (player-seek 0)
            (pq-play* pq prev)))

      ;; Handle first song in pq
      (and-let* ((c (pq-current pq)))
        (player-seek 0))))

;; ==================== thread-safety ====================
(define (with-pq-mutex proc)
  ;; this ain't pretty
  (lambda (pq . args)
    ((with-mutex-lock (pq-mutex pq) (lambda () (apply proc (cons pq args)))))))

(begin
  (define pq-ref   (with-pq-mutex pq-ref*))
  (define pq-add   (with-pq-mutex pq-add*))
  (define pq-add-list   (with-pq-mutex pq-add-list*))
  (define pq-del   (with-pq-mutex pq-del*))
  (define pq-clear (with-pq-mutex pq-clear*))
  (define pq-next  (with-pq-mutex pq-next*))
  (define pq-prev  (with-pq-mutex pq-prev*))

  (define pq-play  (with-pq-mutex pq-play*))
  (define pq-play-next  (with-pq-mutex pq-play-next*))
  (define pq-play-prev  (with-pq-mutex pq-play-prev*)))

(test-group
 "playqueue"

 (let ((pq (make-pq)))
   (test "initially empty" '() (pq-list pq))
   (let ((new-item (pq-add* pq `((turi . "a")))))
     (test "1 item added" (list new-item) (pq-list pq))
     (pq-del* pq new-item))

   (let ((i1 (pq-add* pq `((turi . "1"))))
         (i2 (pq-add* pq `((turi . "2")))))
     (test "2 items: add" (list i1 i2) (pq-list pq))
     (pq-del* pq i1)
     (test "1 items: delete" (list i2) (pq-list pq)))

   (pq-add* pq `((turi . "x")))
   (pq-add* pq `((turi . "y")))
   (pq-clear* pq)
   (test "pq-clear" '() (pq-list pq))

   (test-error (pq-add* pq `((id . "hello") (turi . "a"))))
   (test-error (pq-del* pq `((id . "no existe")))))

 (test "no pq-current" #f (pq-next* (make-pq)))
 (test "no pq-current" #f (pq-prev* (make-pq)))

 (let ((pq (make-pq `( ((id . "a"))
                       ((id . "b"))
                       ((id . "c")) )
                    ;; current:
                    `((id . "b")))))
   (test "pq-next*" `((id . "c")) (pq-next* pq))
   (test "pq-prev*" `((id . "a")) (pq-prev* pq)))


 ;; pq-next
 (let ((pq (make-pq '( ((id . "a")) ((id . "b")) )
                    ;; current
                    '((id . "a")))))
   (test "next" '((id . "b")) (pq-next* pq))

   (pq-current-set! pq '((id . b)))
   (test "false if no next"
         #f (pq-next* pq))
   (test "  unless forced"
         '((id . "a")) (pq-next* pq #t))
   (pq-loop-set! pq #t)
   (test "  or pq-loop is true"
         '((id . "a")) (pq-next* pq #t))

   (pq-current-set! pq #f)
   (test "first if current #f"
         '((id . "a")) (pq-next* pq)))


 (let ((pq (make-pq `(((id . "a"))) '((id . "a")))))
   (test "past last is #f"
         #f (pq-next* pq))
   (test "before first is #f"
         #f (pq-prev* pq)))

 (test-error (pq-play* (make-pq) `((id . "a")))))
)

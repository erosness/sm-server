(module rest-player (player-seek-thread
                     spotify-monitor-thread
                     *pq*)

(import chicken scheme data-structures)

(import player
        rest ;; <-- (rest-server-port)
        playqueue)
(use test restlib clojurian-syntax ports
     srfi-18 extras posix srfi-1
     medea matchable)

(import notify incubator)

(define *pq* (make-pq))

(define ((change-callback path) oldval newval)
  (send-notification path newval))

(pq-add-current-change-listener
 *pq* (change-callback "/v1/player/current"))



;; alist of position, duration, paused etc (or '() if nothing is
;; playing)
(define (player-pos-info)
  (if (player-pos) ;; <- active cplay?
      `((pos .      ,(player-pos))
        (duration . ,(player-duration))
        (paused .   ,(player-paused?)))
      '()))


(define (player-information #!optional (current (pq-current *pq*)))
  (alist-merge current
               (player-pos-info)
               `((loop . ,(pq-loop? *pq*)))))

;; Manipulate current track.
;; POST: Looks for three keys; turi, paused, pos.
;; If turi is present adds this item to pq and starts playing.
;; If paused is present, toggles pause state
;; If pos is present, seek to that position
;; If loop is present, toggles loop state of pq
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

          ;; Change loop?
          (and-let* ((loop (assoc 'loop json-request)))
            (pq-loop?-set! *pq* (cdr loop)))

          ;; Set and NOTIFY new current value
          (let ((new-current (player-information (alist-merge current
                                                              json-request))))
            (pq-current-set! *pq* new-current)
            new-current))
        ;;else
        (player-information))))

;; Adds an item to the back of the playqueue
;; Returns: the passed in item with a unique id added
(define-handler /v1/player/pq/add
  (lambda ()
    (let* ((json (current-json))
           ;; either add a single track or a list of tracks
           (jsonlist (vector->list
                      (if (vector? json)
                          json
                          (vector json)))))
      (list->vector (pq-add-list *pq*
                                 ;; HACK: delete loop cause it belongs
                                 ;; to pq's not tracks. see #99.
                                 (map (cut alist-delete 'loop <>) jsonlist))))))


;; Removes and item referenced by id from the playqueue
;; Does nothing if id is not found in playqueue
(define-handler /v1/player/pq/del
  (lambda () (and-let* ((json (current-json))
                   ((alist-ref 'id json)))
          (pq-del *pq* json)
          `((status . "ok")))))


;; Removes every item from the playqueue and stops the player
(define-handler /v1/player/pq/clear
  (lambda () (pq-clear *pq*)
     (player-quit)
     `((status . "ok"))))


;; Returns the playqueue
(define-handler /v1/player/pq (lambda () (list->vector (pq-list *pq*))))

(define-handler /v1/player/next
  (lambda ()
    (pq-play-next *pq* #t)
    (player-information)))

(define-handler /v1/player/prev
  (lambda ()
    (pq-play-prev *pq*)
    (player-information)))

;; ==================== seek position hearbeat ====================
(import notify)
(use looper medea)


;; do this on every player hearbeat interval
(define (player-thread-iteration)
  (if (playing?) ;; running and not paused?
      (send-notification "/v1/player/pos"
                         (player-pos-info))))

(define player-seek-thread
  (thread-start!
   (make-thread
    (->> (lambda () (player-thread-iteration))
         (loop/interval 1)
         (loop/exceptions (lambda (e) (pp `(error: ,(current-thread)
                                              ,(condition->list e))) #t))
         (loop))
    "player-seek-thread")))

;; (thread-terminate! player-thread)
;; (thread-state player-thread)




;; don't block while reading anything from port p. port p must have an
;; associated filedescriptor.
(define (make-nonblocking-input-port p)
  (make-input-port (lambda ()
                     (thread-wait-for-i/o! (port->fileno p))
                     (read-char p))
                   (lambda () (char-ready? p))
                   (lambda () (close-input-port p))))



(define (playing&active? event)
  (and (alist-ref 'playing? event)
       (alist-ref 'active? event)))


;; send a pretend-current notification to our apps. should keep
;; player-pane in sync with what Spotify is doing.
(define (spotify-notification event)
  (pq-current-set! *pq* `((title . ,(alist-ref 'track event))
                          (subtitle . ,(alist-ref 'artist event))
                          (image . ,(alist-ref 'image event))
                          (type . "spotify")
                          (pos . 0)
                          (duration . ,(* 0.001 (alist-ref 'duration_ms event)))
                          (paused . ,(not (playing&active? event))))))



(define (run-monitor-thread name body #!optional (interval 1))
  (thread-start!
   (->> body
        (loop/interval 1)
        (loop/exceptions (lambda (e)
                           (pp `(,(current-thread),(condition->list e)))
                           (thread-sleep! 10)
                           #t)) ; <-- keep going
        (loop)
        ((flip make-thread) name))))



;; watch if spotify is playing. if it is, we pause our own cplay and
;; we "sneak" spotify album-cover art and player state in there using
;; spotify-notification.
(begin
  (handle-exceptions e (void) (thread-terminate! spotify-monitor-thread))
  (define spotify-monitor-thread
    (run-monitor-thread
     "spotify-monitor"
     (lambda ()
       (let ((event (call-with-input-pipe
                     "spotifyctl 7879 event"
                     (o read make-nonblocking-input-port))))
         (pp `(info ,(current-thread) event ,event))
         (if (eof-object? event)
             (thread-sleep! 10)
             (when (playing&active? event)
               (player-pause)
               (spotify-notification event))))))))

;; Read and broadcast DAB dynamic label if dab is running
;; Note that the dynamic label is only broadcasted through the notify
;; socket, you won't get it from
(begin
  (use dab)
  (handle-exceptions e (void) (thread-terminate! dab/fm-notifier))
  (define dab/fm-notifier
    (run-monitor-thread
     "dab/fm-notifier"
     (lambda ()
       (and-let* (((pq-current *pq*))
                  (subtitle (match (alist-ref 'type (pq-current *pq*))
                              ("dab" (dab-dls))
                              ("fm"  (fm-radio-text))
                              (else #f)))
                  (content (alist-merge (player-information) `((subtitle . ,subtitle)))))
         (send-notification "/v1/player/current" content))
       #t) ; <-- keep going
     )))
)




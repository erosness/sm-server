(module* rest-mplayer (;; player-seek-thread
;;                     player-information
;;                     spotify-monitor-thread
                     /v1/mplayer/current
                     *mpq*)

(import chicken scheme data-structures)

(import player
        rest ;; <-- (rest-server-port)
        playqueue)
(use test restlib clojurian-syntax ports
     srfi-18 extras posix srfi-1 srfi-13
     medea matchable irregex matchable)

(import notify incubator)
(define *mpq* (make-pq)) ;; Multiroom play queue.

(define ((change-callback path) oldval newval)
  (send-notification path newval))

(pq-add-current-change-listener
 *mpq* (change-callback "/v1/mplayer/current"))



(define (local-ip)
  (irregex-match-substring
   (irregex-search
    '(: "addr:" (=> ip (or "192" "10") ;; assuming your LAN has this ip
                    (= 3 "." (** 1 3 numeric))) )
    (with-input-from-pipe "ifconfig|grep inet" read-string))
   'ip))



;; alist of position, duration, paused etc (or '() if nothing is
;; playing)
(define (player-pos-info)
  (if (player-pos) ;; <- active cplay?
      `((pos .      ,(player-pos))
        (duration . ,(player-duration))
        (paused .   ,(player-paused?)))
      '()))


(define (player-information #!optional (current (pq-current *mpq*)))
  (alist-merge current
               (player-pos-info)
	       `((uid_leader . ,(local-ip)))
               `((loop . ,(pq-loop? *mpq*)))
	       `((status . "Ok"))
	       ))


;; Tracks that should not be added to the playqueue,
;; ie. maestro capture devices.
(define (play-direct? item)
  (let ((type (alist-ref 'type item)))
    (match type
      ("line-in" #t)
      ("toslink" #t)
      ("bt"      #t)
      (else      #f))))


(define-handler /v1/mplayer/follower
  ( lambda()
    (if (current-json)
	(let ((uid_leader (alist-ref 'uid_leader (current-json))))
	  (play-follower uid_leader)
	  (print "UID: " uid_leader)
	  `((uid_follower . ,(local-ip))
	    (status . "Ok")
	    )
	  )
	`((uid_follower . ,(local-ip))
	  (status . "Fail")
	  )
	)))



(define-handler /v1/mplayer/addfollower
  ( lambda()
    (if (current-json)
	(let ((uid_follower (alist-ref 'uid_follower (current-json))))
	  (play-addfollower uid_follower)
	  (print "UID: " uid_follower)
	  (player-information)	  
	  )
	`((status . "Fail"))
	)))

(define-handler /v1/mplayer/removefollower
  (lambda()
    (if (current-json)
	(let ((uid_follower (alist-ref 'uid_follower (current-json))))
	  (play-rmfollower uid_follower)
	  (player-information)
	  )
	`((status . "Fail"))
	)))

(define-handler /v1/mplayer/quit
  (lambda()
    (player-quit)
    (player-information)
    ))


;; Note: [pq-play with #f]
;; in the player/current handler we update 'current' at the very end of any
;; POST request. passing #f to pq-play prevents it from updating
;; current (since we're going to do this anyway when all the requested
;; updates have been applied

;; Manipulate current track.
;; POST: Looks for three keys; turi, paused, pos.
;; If turi is present adds this item to pq and starts playing.
;; If paused is present, toggles pause state
;; If pos is present, seek to that position
;; If loop is present, toggles loop state of pq
;; Returns: new value of current
;; GET: returns value of current with updated pos.
(define-handler /v1/mplayer/current
  (lambda ()
    (if (current-json)
        (let* ((json-request (current-json))
               ;; returns the first track with the same id as json
               ;; request or #f if none
               (existing (pq-ref *mpq* json-request))
               ;; the currently playing track or #f if none
               (current (pq-current *mpq*)))

		  (print "mplayer/current incoming request")
		  (pp (current-json))

          ;; Change track?
          (if (or (alist-ref 'turi json-request)
                  (alist-ref 'id json-request))
              (if existing
                  ;; if the requested track is already in the queue, start playing it
                  (begin
                    (print "mpq: playing track which already exists in mpq")
                    (pq-leader-play *mpq* existing #f) ;; - see [pq-play with #f]
                    (set! current existing))
                  (or
                   ;; Should this track be played without being added
                   ;; to the playqueue?
                   (and-let* (((play-direct? json-request)))
                     (pq-leader-play *mpq* json-request #f)
                     (set! current json-request))

                   (begin
                     ;; if the requested track is _not_ already in the
                     ;; queue, delete all tracks following it, add requested and
                     ;; play it.
                     (and-let* ((played (pq-drop-after *mpq* current))
                                ((pq-list-set! *mpq* played))
                                (requested (pq-add *mpq* json-request)))
                       (pq-leader-play *mpq* requested #f) ;; - see [pq-play with #f]
                       (set! current requested)))

                   ;; no current, add requested and play it
                   (let ((requested (pq-add *mpq* json-request)))
                     (pq-leader-play *mpq* requested #f) ;; - see [pq-play with #f]
                     (set! current requested)))))

          ;; Change pos?
          (and-let* ((pos (assoc 'pos json-request))
					 (current)
					 (current-duration (alist-ref 'duration current))
                     ;; Don't allow seek on infinite streams
                     ((>= current-duration 0)))
            (print "seeking track to position " (cdr pos))
            (player-seek (cdr pos)))

          ;; Change paused?
          (and-let* ((pause (assoc 'paused json-request)))
            (if (cdr pause) (player-pause) (player-unpause)))

          ;; Change loop?
          (and-let* ((loop (assoc 'loop json-request)))
            (pq-loop?-set! *mpq* (cdr loop)))

          ;; Set and NOTIFY new current value
          (let ((new-current (player-information current)))
            (pq-current-set! *mpq* new-current)
            new-current))
        ;;else
        (player-information))))

;; Adds an item to the back of the playqueue
;; Returns: the passed in item with a unique id added
(define-handler /v1/mplayer/pq/add
  (lambda ()
    (let* ((json (current-json))
           ;; either add a single track or a list of tracks
           (jsonlist (vector->list
                      (if (vector? json)
                          json
                          (vector json)))))
      (list->vector (pq-add-list *mpq*
                                 ;; HACK: delete loop cause it belongs
                                 ;; to pq's not tracks. see #99.
                                 (map (cut alist-delete 'loop <>) jsonlist))))))


;; Removes and item referenced by id from the playqueue
;; Does nothing if id is not found in playqueue
(define-handler /v1/mplayer/pq/del
  (lambda () (and-let* ((json (current-json))
                   ((alist-ref 'id json)))
          (pq-del *mpq* json)
          `((status . "ok")))))


;; Removes every item from the playqueue
(define-handler /v1/mplayer/pq/clear
  (lambda () (pq-clear *mpq*)
     `((status . "ok"))))


;; Returns the playqueue
(define-handler /v1/mplayer/pq (lambda () (list->vector (pq-list *mpq*))))

(define-handler /v1/mplayer/next
  (lambda ()
    (pq-play-next *mpq* #t)
    (player-information)))

(define-handler /v1/mplayer/prev
  (lambda ()
    (pq-play-prev *mpq*)
    (player-information)))

;; ==================== seek position hearbeat ====================
(import notify)
(use looper medea)


;; do this on every player hearbeat interval
(define (player-thread-iteration)
  (if (playing?) ;; running and not paused?
      (send-notification "/v1/mplayer/pos"
                         (player-pos-info))))

(define player-seek-thread
  (thread-start!
   (make-thread
    (->> (lambda () (player-thread-iteration))
         (loop/interval 1)
         (loop/exceptions (lambda (e) (pp `(error: ,(current-thread)
                                              ,(condition->list e))) #t))
         (loop))
    "mplayer-seek-thread")))

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
  (pq-current-set! *mpq* `((title . ,(alist-ref 'track event))
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
       (and-let* (((pq-current *mpq*))
                  (subtitle (match (alist-ref 'type (pq-current *mpq*))
                              ("dab" (dab-dls))
                              ("fm"  (fm-radio-text))
                              (else #f)))
                  (content (alist-merge (player-information) `((subtitle . ,subtitle)))))
         (send-notification "/v1/mplayer/current" content))
       #t) ; <-- keep going
     )))

)

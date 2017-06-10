(module rest-group *

(import chicken scheme data-structures)

(use test restlib clojurian-syntax ports
     srfi-18 extras posix srfi-1 srfi-13
     medea matchable)

(import notify incubator)
;; ==================== define group list ====================
(define-record-type group (make-group icon name uri type serial)
  group?
  (icon group-icon group-icon-set!)

(define-record-type rect
  (make-rect x y w h)
  rect?
  (x rect-x rect-x-set!)
  (y rect-y rect-y-set!)
  (w rect-w rect-w-set!)
  (h rect-h rect-h-set!)
)

;; -type group-element (%make-pq list mutex current loop?)
;;  pq?
;;  (list pq-list %pq-list-set)
;;  (mutex pq-mutex)
;;  (current %pq-current)
;;  (loop? pq-loop? pq-loop?-set!))


;; ==================== handlers ====================
(define-handler /v1/group/test
  (lambda ()
    (print "/v1/group/test access")))


;; ==================== seek position hearbeat ====================
(import notify)
(use looper medea)



;; don't block while reading anything from port p. port p must have an
;; associated filedescriptor.
(define (make-nonblocking-input-port p)
  (make-input-port (lambda ()
                     (thread-wait-for-i/o! (port->fileno p))
                     (read-char p))
                   (lambda () (char-ready? p))
                   (lambda () (close-input-port p)))) )





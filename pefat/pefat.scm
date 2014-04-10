;;; modify default test-handler to print summary instead of just
;;; FAIL/PASS. this makes it easier to spot if something went wrong
;;; with lots of tests without having to halt distract the flow.
;;;
;;; Usage: (use pefat) and your (test 1 1) will print differently.
;;;
;;; PEFAT: Peders Egg, Favorite Acronym and Testing
(module pefat *
(import chicken scheme)

(use test data-structures)

;; private helpers
(define %test-tests (make-parameter 0))

;; defonce
(define %old-test-handler
  (or (condition-case %old-test-handler ((exn runtime) #f))
      (current-test-handler)))

(define (green x) (string-append "\x1B[32m" (->string x) "\x1B[0m"))
(define (red x)   (string-append "\x1B[31m" (->string x) "\x1B[0m"))

(define (pass)
  (let* ((fails (test-failure-count))
         (color (if (> fails 0) red green)))
    (color (conc fails "/" (%test-tests)))))

(current-test-handler
 (lambda (status expect x info)

   ;; this is very very silly. we need to put this here. without
   ;; proper counting information, default-test-applier will print the
   ;; header on every test-case (not just the first in the group).
   (cond ((current-test-group) =>
          (lambda (g)
            (test-group-inc! g 'count)
            (test-group-inc! g status))))

   (define (inc parameter)
     (parameter (+ 1 (parameter))))

   (inc %test-tests)

   (cond ((eq? status 'PASS) (print "[" (pass) "]"))
         (else (%old-test-handler status expect x info) ))))

;; no need for end-of-group report
(current-test-group-reporter (lambda (group) (void)))

)

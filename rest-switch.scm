(use test restlib looper clojurian-syntax)

(import rest process-cli)

;; Initial position is set to 0. Maybe pull-up should be set to weak pull down?
(define %switch 0)

(define (switch! setting)
    (set! %switch setting)
    (if (= 1 setting)
      (process-cli "sh" '("-c" "solo-switch-on.sh") (lambda () (print "xyzzy")))
      (process-cli "sh" '("-c" "solo-switch-off.sh") (lambda () (print "xyzzy")))))

;; This is just a local readback, mo real pin reading
(define (switch?) %switch)

(define-handler /v1/switch
  (lambda ()
    (if (current-json)
      (let ((setting (alist-ref 'value (current-json))))
        (if (and (integer? setting)
                (or (= 0 setting)
                (= 1 setting)))
	  (begin
            (switch! setting)
            '((status . "ok")))
          '((status . "Fail"))))
      `((value . ,(switch?))))))


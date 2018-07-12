(use test restlib looper clojurian-syntax)

(import rest process-cli)

(define %switch 0)

(define (switch! setting)
    (set! %switch setting)
    (print "Value=" setting))

(define (switch?)
  (print "Switch getting")
  %switch)


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


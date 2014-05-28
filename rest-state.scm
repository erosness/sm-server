
(define-syntax define-state
  (ir-macro-transformer
   (lambda (x e t)
     (let* ((key (e (cadr x)))
            (path (e (string->symbol (conc *store-base-url* key)))))
       `(define-handler ,path
          (lambda () (if (current-json)
                    (let ((json-request (current-json)))
                      (state-set! (quote ,key) json-request)
                      json-request)
                    (state-ref/default (quote ,key) '()))))))))

;; example state
(define-state foo)

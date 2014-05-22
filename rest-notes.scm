(module rest-notes ()

(import chicken scheme data-structures)

(use irregex restlib)
(import rest turi)


;; taken from http://trac.ffmpeg.org/wiki/FancyFilteringExamples
(define-turi-adapter note->turi "notes"
  (lambda (uri)
    `((url . ,(irregex-replace/all
               "([,;])"
               (conc
                "aevalsrc="
                ;; floor(t): 0 0 0 0 0 ... 1 1 1 1 1 ... 2 2 2 2 2
                ;;  => set a random key when floor(t) changes
                "if(eq(floor(t),ld(2)),"
                "st(0,random(4)*3000+1000));"
                ;; the next value to compare floor(t) with
                "st(2,floor(t)+1);"
                ;; mod(t,1) makes t always in the range [0;1) for each key
                "st(1,mod(t,1));"
                ;; 0.6*... + 0.4*... for  echo effect
                ;; exp() to mitigate the sound according to the time
                "(0.6*sin(1*ld(0)*ld(1))+"
                "0.4*sin(2*ld(0)*ld(1)))*exp(-4*ld(1))")
               (lambda (m) (conc "\\" (irregex-match-substring m 1)))))
      (format . "lavfi"))))

(define-handler /v1/catalog/notes (pagize (lambda () (list (note->turi 0)))))

)

;; (play! (play-command "tr://localhost:5060/t2s?type=notes&id=0"))
;; (player-quit)

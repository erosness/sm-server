(use irregex)

;; TODO: make this less tricky!
(define-handler "/search/notes" (pagize (argumentize (querify `(((turi . "tr://notes/")))) 'q)))

;; taken from http://trac.ffmpeg.org/wiki/FancyFilteringExamples
(define-audio-host "notes"
  (lambda (uri)
    (irregex-replace/all
     "([,;])"
     (conc
      "cplay -f lavfi \"aevalsrc="
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
      "0.4*sin(2*ld(0)*ld(1)))*exp(-4*ld(1))"
      "\"")
     (lambda (m) (conc "\\" (irregex-match-substring m 1))))))

;; (play! (play-command "tr://notes/"))
;; (player-quit)

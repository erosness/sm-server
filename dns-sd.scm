;;; use avahi/dns-sd to register a service. it spawns a sub-process
(use posix)

(define service-type/cube-browser  "_cube-browser._tcp")
(define service-type/cube-pq            "_cube-pq._tcp")

(define (discovery-command/mac name port servicetype)
  (conc "dns-sd -R \""        name "\" " servicetype " local " port))

(define (discovery-command/linux name port servicetype)
  (conc "avahi-publish -s \"" name "\" " servicetype " "       port))

(define (discovery-command name port servicetype)
  ( (cond ((feature? linux:) discovery-command/linux)
          ((feature? macosx:) discovery-command/mac)
          (else (error "I don't know what OS this is! " (features))))

    name port servicetype))

;; start announce-process asynchronously. returns a procedure which
;; will stop it.
(define (dns-sd-register name port servicetype)
  (print "running " (discovery-command name port servicetype))
  (let ((pid (process-run (discovery-command name port servicetype))))
    (set! -last-dns-sd-pid- pid)
    (define (kill-announcer)
      (warning "killing service announce daemon" pid)
      (process-signal pid))
    (on-exit kill-announcer)
    kill-announcer))

;; (define dns-sd-unregister! (dns-sd-register "repl service test" 6600 service-type/cube-browser))
;; (dns-sd-unregister!)

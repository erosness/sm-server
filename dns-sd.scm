;;; use avahi/dns-sd to register a service. it spawns a sub-process
(use posix)

(define (discovery-command/mac name port)
  (conc "dns-sd -R \""        name "\" _cube._tcp local " port))

(define (discovery-command/linux name port)
  (conc "avahi-publish -s \"" name "\" _cube._tcp "       port))

(define (discovery-command name port)
  ( (cond ((feature? linux:) discovery-command/linux)
          ((feature? macosx:) discovery-command/mac)
          (else (error "I don't know what OS this is! " (features))))

    name port))

;; start announce-process asynchronously. returns a procedure which
;; will stop it.
(define (dns-sd-register name port)
  (let ((pid (process-run (discovery-command name port))))
    (lambda ()
      (warning "killing service announce daemon" pid)
      (process-signal pid))))

;; (dns-sd-register "repl service test" 6600)

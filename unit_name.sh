#!/bin/csi -s
;; -*- scheme -*-

(use posix)

(define default-name
  (if (file-exists? "/etc/os-release")
      "Solo2"
      "Maestro"))

(define (read-name filename)
  (if (file-exists? filename)
      (alist-ref 'name (with-input-from-file filename read))
      default-name))

(define (use-name filename)
  (let ((name (read-name filename)))
    (if (and name (< 0 (string-length name)))
      name
      default-name)))

(print (use-name  "/data/data/speaker-icon-5055-store.scm"))

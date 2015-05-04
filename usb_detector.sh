#!/system/bin/csi -s
;; -*- scheme -*-

(use posix matchable srfi-13)

(define (mount-msg? msg)
  (string-contains msg "to 4 (Mounted)"))

(define (umount-msg? msg)
  (string-contains msg "to 0 (No-Media)"))

(define (create-media-db)
  ;; Make sure the media db does not already exist,
  ;; without this we get issues if the usb device was changed while
  ;; the unit was turned off.
  (delete-media-db)
  (print "creating mediadb")
  (system "mediascanner -s /mnt/udisk -d /data" ))

(define (delete-media-db)
  (print "deleting mediadb")
  (system "rm /data/files.db"))

(define (process-line line)
  (match line
    (#!eof #f)
    ((? mount-msg?)  (create-media-db))
    ((? umount-msg?) (delete-media-db))
    ;; Ignore, but keep going
    (else #t)))

(with-input-from-pipe
 "vdc monitor"
 (lambda ()
   (print "Connected to Vold")
   (let loop ()
     (let ((line (read-line)))
       (if (process-line line)
           (loop))))))


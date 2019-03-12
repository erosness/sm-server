#!/system/bin/csi

(use posix nanomsg)

(define nnsock-gst
  (let ((nnsock-gst (nn-socket 'pair)))
    (nn-connect nnsock-gst "ipc:///data/nanomessage/playcmd.pair")
    (nn-send nnsock-gst "print_mib\n")
    (print "Before nn-recv on nnsock-gst")
    (print (nn-recv nnsock-gst))
    (print "After nn-recv on nnsock-gst")
    nnsock-gst))


(define (nano-get-msg socketgst)
  (let ((sock socketgst))
    (nn-recv* sock nn/dontwait)))

(define (slow-get-msg socket)
  (sleep 1)
  (nano-get-msg socket))



  (print ": " (slow-get-msg nnsock-gst))
  (nn-send nnsock-gst "play http://listen.181fm.com/181-70s_128k.mp3?noPreRoll=true\n")
  (print ": " (slow-get-msg nnsock-gst))
  (print ": " (slow-get-msg nnsock-gst))
  (print ": " (slow-get-msg nnsock-gst))
  (nn-send nnsock-gst "pos\n")
  (print ": " (slow-get-msg nnsock-gst))
  (nn-send nnsock-gst "paused?\n")
  (print ": " (slow-get-msg nnsock-gst))
  (nn-send nnsock-gst "pos\n")
  (print ": " (slow-get-msg nnsock-gst))
  (nn-send nnsock-gst "state?\n")
  (print ": " (slow-get-msg nnsock-gst))
  (nn-send nnsock-gst "mrstate\n")
  (print ": " (slow-get-msg nnsock-gst))
  (nn-send nnsock-gst "pos\n")
  (print ": " (slow-get-msg nnsock-gst))
  (print ": " (slow-get-msg nnsock-gst))

(exit)

#lang racket

(require racket/tcp
	 "common.rkt")

(provide (all-defined-out))

(define in "input port for server connection")
(define out "output port for server connection")

(define send-to-server	      ;; write to TCP output port and
  (lambda (message)	      ;; flush to transmit message immediately
    (write message out)
    (flush-output out)))

(define receive-from-server   ;; read a message from the TCP input port
  (lambda ()
    (read in)))


(define disconnect-client
  (lambda ()
    (kill-thread UIT)            ;; kill user interaction thread 
    (kill-thread SIT)            ;; and server interaction thread
    (close-input-port in)	 ;; and close connection to server
    (close-output-port out)))


(define get-host
  (lambda ()
    (display (format "Host (~a): " (server-host)))
    (let ((host (read-line)))
      (when (not (equal? host ""))
	    (server-host host))
      (server-host))))

(define get-port
  (lambda ()
    (display (format "Port (~a): " (server-port)))
    (let ((port (read-line)))
      (when (not (equal? port ""))
	    (server-port (string->number port)))
      (server-port))))

(define connect-to-server
  (lambda (host port)
    (let-values (([server-in server-out] (tcp-connect host port)))
      (set! in server-in)
      (set! out server-out))))

(define connected?
  (lambda ()
    (and (input-port? in)
	 (not (port-closed? in))
	 (output-port? out)
	 (not (port-closed? out)))))

(define message-from-server (make-parameter "message-from-server"))

(define message-from-server?
  (lambda (message) (equal? message (message-from-server))))


(define message-from-client-thread (make-parameter "message-from-client-thread"))

(define message-from-client-thread?
  (lambda (message) (equal? message (message-from-client-thread))))


(define message-from-user (make-parameter "message-from-user"))

(define message-from-user?
  (lambda (message) (equal? message (message-from-user))))


(define user-quit (make-parameter "user-quit"))

(define user-quit?
  (lambda (message) (equal? message (user-quit))))

(define UIT "user interaction thread")
(define SIT "server interaction thread")

(define start-UIT
  (lambda ()
    (set! UIT (thread user-interaction-thread))))

(define start-SIT
  (lambda ()
    (set! SIT (thread server-interaction-thread))))
    
(define start-client  ;; start the user interaction thread,
  (lambda ()	      ;; and process messages from server
    (start-UIT)	      
    (with-handlers 
     ((user-quit?
       (lambda (exn) 
	 (send-to-server (client-quit-message))
	 (send-to-UIT-from-client-thread 
	  "User quit.  Server connection terminated.")
	 (disconnect-client)))
      (exn:break?
       (lambda (exn) 
	 (send-to-server (client-disconnected-message))
	 (send-to-UIT-from-client-thread 
	  "User break.  Server connection terminated.")
	 (disconnect-client)))
      (exn:fail?
       (lambda (exn) 
	 (send-to-server (client-disconnected-message))
	 (send-to-UIT-from-client-thread 
	  (format "~a.  Server connection terminated." (exn-message exn)))
	 (disconnect-client))))
     (start-SIT)
     )))

(define send-to-UIT-from-server
  (lambda (message)
    (when (thread-running? UIT)
      (thread-send UIT (message-from-server))
      (thread-send UIT message))))

(define send-to-UIT-from-client-thread
  (lambda (message)
    (when (thread-running? UIT)
      (thread-send UIT (message-from-client-thread))
      (thread-send UIT message))))

(define server-interaction-thread
  (lambda ()
    (when (not (port-closed? in))
	  (let ((message (receive-from-server)))
	    (send-to-UIT-from-server message)	 
	    (sleep 0.10)
	    (when (server-shutdown? message)
		  (disconnect-client))
	    (server-interaction-thread)))))

(define user-interaction-thread
  (lambda ()
    (let ((message (thread-try-receive)))  ; (display (format "message: ~a\n" message))
      (when (message-from-user? message)
	(send-to-server (thread-receive)))
      (when (message-from-server? message)
	((handle-message-from-server) (thread-receive)))
      (when (message-from-client-thread? message)
	((handle-message-from-client-thread) (thread-receive)))
      (sleep 0.10)
      (user-interaction-thread))))

(define handle-message-from-server  ;; should be overridden by application
  (make-parameter (lambda (message) 'do-nothing)))

(define handle-message-from-client-thread  ;; should be overridden by application
  (make-parameter (lambda (message) 'do-nothing)))

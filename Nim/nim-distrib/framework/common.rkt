#lang racket

(provide (all-defined-out))

(define server-host (make-parameter "localhost"))

(define server-port (make-parameter 2155))


(define syn-message "connection synch message")

(define syn?
  (lambda (m) (equal? m syn-message)))


(define ack-message "connection acknowledge message")

(define ack?
  (lambda (m) (equal? m ack-message)))


(define syn-ack-exception "connection syn-ack exception")

(define syn-ack-exception?
  (lambda (m) (equal? m syn-ack-exception)))


(define timing-message "timing message")

(define timing?
  (lambda (m) (equal? m timing-message)))


;; ************************************************************
;; System messages between server and client

(define connection-failed-message 
  "Connection failed. Server may not be running. Also check host and port.")

(define connection-failed?
  (lambda (m) (equal? m connection-failed-message)))


(define client-disconnected-message "Client disconnected.")

(define client-disconnected?
  (lambda (m) (equal? m client-disconnected-message)))


(define server-shutdown-message "Unexpected server shutdown.")

(define server-shutdown?
  (lambda (m) (equal? m server-shutdown-message)))


(define client-timeout-message "Client timed out due to inactivity.")

(define client-timeout?
  (lambda (m) (equal? m client-timeout-message)))


(define client-quit-message "Client quits.")

(define client-quit?
  (lambda (m) (equal? m client-quit-message)))


(define fatal-message?
  (lambda (m)                    
    (or (client-disconnected? m)
	(server-shutdown? m)
	(client-timeout? m)
	(client-quit? m))))

(define system-message?           ;; in the future a system message
  (lambda (m)                     ;; may not be fatal
    #f))


;; ************************************************************

(define display-only-message "display-only")

(define display-only?
  (lambda (m) (equal? m display-only-message)))

;; ************************************************************
;; Connection state constant.  Others Can be added by applications.

(define handshake-state "**handshake-state**")

;; ************************************************************
;; Channels for server to server UI communication

(define log-message-channel (make-channel))
(define client-change-channel (make-channel))


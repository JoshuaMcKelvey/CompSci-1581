#lang racket/gui

(require "client.rkt" "common.rkt")

(provide (all-defined-out) )

(define client-frame%
  (class frame%
    (super-new)
    (define (on-close)
      (when (thread? UIT)
	(disconnect-client)))
    (augment on-close)))

;; (define frame (new frame% 
;; 		   (label "Client")
;; 		   ))

(define frame (new client-frame% 
		   (label "Client")
		   ))

(define dummy-frame (new frame% [label "Dummy Frame"])) ; for holding containees for later reparenting

(define gui-width 400)

(define connect-prompt 
  "To connect to the server, enter host and port, then click Connect.")

;; **************************************************************************

(define connection-panel (new horizontal-panel% [parent frame]))

(define host-field (new text-field%
                        [parent connection-panel]
                        [label "Host"]))

(define port-field (new text-field%
                        [parent connection-panel]
                        [label "Port"]))

(define button-panel (new vertical-panel% 
                          [parent connection-panel]
                          [alignment '(center center)]))

(define connect-button (new button%
                            [parent button-panel]
                            [label "Connect"]
                            [callback (lambda (button event) (connect))]))

(define disconnect-button (new button%
                               [parent button-panel]
                               [label "Disconnect"]
                               [callback (lambda (button event) (disconnect))]
                               [enabled #f]))

;; (define quit-button (new button%
;; 			 (parent button-panel)
;; 			 (label "Quit")
;; 			 (callback (lambda (button event) (quit)))))

(send host-field set-value (server-host))
(send port-field set-value (number->string (server-port)))

(define connect
  (lambda ()
    (let ((host (send host-field get-value))
          (port (string->number (send port-field get-value))))
      (with-handlers ([exn:fail:network?
                       (lambda (exn)
			 (system-update (string-append 
					 connection-failed-message
					 "\n"
					 (exn-message exn)))
                         )])
	(connect-to-server host port)
        (start-client)
	(send send-panel show #t)
        (send send-button show #t)
        (send disconnect-button enable #t)
        (send connect-button enable #f)
	(send system-panel show #f)
	))))

(define reset
  (lambda ()
    (send send-panel show #f)
    (send send-button show #f)
    (send disconnect-button enable #f)	
    (send connect-button enable #t)
    (send receive-field set-value connect-prompt)))

(define disconnect
  (lambda ()
    (reset)
    (disconnect-client)))

(define quit
  (lambda ()
    
    (exit)))

;; **************************************************************************

(define receive-panel (new vertical-panel%
                           [parent frame]
                           [alignment '(left top)]))

(define receive-label (new message%
                           [parent receive-panel]
                           [label "Received Message:"]))

(define receive-field (new text-field%
                           [parent receive-panel]
                           [label #f]
                           [style '(multiple)]
                           [min-width gui-width]
                           [min-height 50]))


;; **************************************************************************

(define send-panel (new vertical-panel%
                        [parent frame]
                        [alignment '(left top)]))

(define send-label (new message%
                        [parent send-panel]
			[min-width gui-width]
                        [label "Your Message:"]))

(define send-component (new text-field%           ;; can be overridden
			    [parent send-panel]
			    [label #f]
			    [style '(multiple)]
			    [min-width gui-width]
			    [min-height 50]))

(define change-send-component
  (lambda (new-component)
    (send send-component reparent dummy-frame)
    (send new-component reparent send-panel)
    (set! send-component new-component)))

;; **************************************************************************

(define send-button 
  (new button%
       [parent frame]
       [label "Send Message"]
       [callback (lambda (button event)
		   (when (thread-running? UIT)
		     (thread-send UIT (message-from-user))
		     (thread-send UIT ((handle-send-click) button event))))]))

;; **************************************************************************

(define system-panel (new vertical-panel%
                           [parent frame]
                           [alignment '(left top)]))

(define system-label (new message%
                           [parent system-panel]
                           [label "SYSTEM MESSAGE:"]))

(define system-field (new text-field%
                           [parent system-panel]
                           [label #f]
                           [style '(multiple)]
                           [min-width gui-width]
                           [min-height 150]))


;; **************************************************************************

(define system-update
  (lambda (m)
    (send system-field set-value m)
    (send system-panel show #t)))

(handle-message-from-server
 (lambda (message)  ; (display (format "handle-message-from-server: message = ~a\n" message))
   (cond ((syn? message) (send-to-server ack-message))
	 ((or (system-message? message)
	      (fatal-message? message))
	  (system-update message)
	  (when (fatal-message? message) 
	     (disconnect)))
	 (else
	  ((handle-message) message)))))
       
(define handle-message  ;; should be overridden by user interface
  (make-parameter (lambda (message) 'do-nothing)))

(define handle-send-click  ;; should be overridden by user interface
  (make-parameter (lambda (button event) 'do-nothing)))

(reset)
(send system-panel show #f)
(send frame show #t)


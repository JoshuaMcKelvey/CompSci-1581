#lang racket

(require "../framework/common.rkt")

(provide (all-defined-out))

;; ******************************************************************

(define make-tagged-message cons)

(define tagged-message-tag car)

(define tagged-message-message cdr)

(define tagged-message?
  (lambda (tag message)
    (and (cons? message)
	 (symbol? (car message))
	 (symbol=? (car message) tag))))

;; ******************************************************************

(define make-welcome-message
  (lambda (message) (make-tagged-message 'Welcome message)))

(define welcome-message?
  (lambda (message) (tagged-message? 'Welcome message)))

;; ******************************************************************

(define make-name-message
  (lambda (message) (make-tagged-message 'Name message)))

(define name-message?
  (lambda (message) (tagged-message? 'Name message)))

;; ******************************************************************

(define make-level-message
  (lambda (message) (make-tagged-message 'Level message)))

(define level-message?
  (lambda (message) (tagged-message? 'Level message)))

;; ******************************************************************

(define make-mode-message
  (lambda (message) (make-tagged-message 'Mode message)))

(define mode-message?
  (lambda (message) (tagged-message? 'Mode message)))

;; ******************************************************************

(define make-flip-message
  (lambda (message) (make-tagged-message 'Flip message)))

(define flip-message?
  (lambda (message) (tagged-message? 'Flip message)))

;; ******************************************************************

(define make-move-message
  (lambda (message) (make-tagged-message 'Move message)))

(define move-message?
  (lambda (message) (tagged-message? 'Move message)))

;; ******************************************************************

(define make-winner-message
  (lambda (message) (make-tagged-message 'Winner message)))

(define winner-message?
  (lambda (message) (tagged-message? 'Winner message)))

;; ******************************************************************

(define make-automate-message
  (lambda (message) (make-tagged-message 'Automate message)))

(define automate-message?
  (lambda (message) (tagged-message? 'Automate message)))

;; ******************************************************************

(define make-strategy-message
  (lambda (message) (make-tagged-message 'Strategy message)))

(define strategy-message?
  (lambda (message) (tagged-message? 'Strategy message)))

;; ******************************************************************

(define make-bad-strategy-message
  (lambda (message) (make-tagged-message 'BadStrategy message)))

(define bad-strategy-message?
  (lambda (message) (tagged-message? 'BadStrategy message)))

;; ******************************************************************

(define make-results-message
  (lambda (message) (make-tagged-message 'Results message)))

(define results-message?
  (lambda (message) (tagged-message? 'Results message)))

;; ******************************************************************

(define make-error-message
  (lambda (message) (make-tagged-message 'Error message)))

(define error-message?
  (lambda (message) (tagged-message? 'Error message)))

;; ******************************************************************

(define nim-message?
  (lambda (message)
    (or (welcome-message? message)
	(name-message? message)
	(level-message? message)
	(mode-message? message)
	(flip-message? message)
	(move-message? message)
	(winner-message? message)
	(automate-message? message)
	(strategy-message? message)
	(bad-strategy-message? message)
	(results-message? message)
	(error-message? message))))

;; ******************************************************************
;; Connection state constants

(define welcome-state "**welcome-state**")

(define name-state "**name-state**")

(define level-state "**level-state**")

(define mode-state "**mode-state**")

(define flip-state "**flip-state**")

(define move-state "**move-state**")

(define winner-state "**winner-state**")

(define automate-state "**automate-state**")

(define strategy-state "**strategy-state**")

(define bad-strategy-state "**bad-strategy-state**")

(define results-state "**results-state**")

;; *******************************************************************
;; A Move Instruction ADT

(define make-move-instruction cons)

(define coins car)

(define pile cdr)

;; *******************************************************************
;; 3-pile Nim game state representation

(define num-piles 3)
(define min-size 6)
(define max-size 12)

(define make-game-state
  (lambda (n m k) (cons k (cons n m))))

(define size-of-pile
  (lambda (game-state pile-number)
    (cond ((= pile-number 3)
	   (car game-state))
	  ((= pile-number 1)
	   (car (cdr game-state)))
	  (else ; pile-number must be 2
	   (cdr (cdr game-state))))))

(define remove-coins-from-pile
  (lambda (game-state n p)
    (let ((c1 (size-of-pile game-state 1))
	  (c2 (size-of-pile game-state 2))
	  (c3 (size-of-pile game-state 3)))
      (cond ((= p 1)
	     (make-game-state (- c1 n) c2 c3))
	    ((= p 2)
	     (make-game-state c1 (- c2 n) c3))
	    (else ;; (= p 3)
	     (make-game-state c1 c2 (- c3 n)))))))

;; *******************************************************************
;; Enhanced Game State ADT Implementation: allows any number of piles.
;; Representation is just a list (p1 p2 ... pn) where n is the number
;; of piles.

;; (define make-game-state
;;   (lambda args args))

;; (define size-of-pile
;;   (lambda (game-state pile-number)
;;     (list-ref game-state (- pile-number 1))))

;; (define remove-coins-from-pile
;;   (lambda (game-state num-coins pile-number)
;;     (let ((pos (- pile-number 1)))
;;       (append (take game-state pos) 
;; 	      (list (- (list-ref game-state pos) num-coins)) 
;; 	      (drop game-state (+ pos 1))))))

;; (define next-game-state
;;   (lambda (game-state move-instruction)
;;     (remove-coins-from-pile game-state (coins move-instruction) (pile move-instruction))))

;; (define game-state-equal?     ;; checks if the two states are the same
;;   (lambda (state-1 state-2)   
;;     (equal? (sort state-1 <)
;; 	    (sort state-2 <))))

(require "test-utilities.scm")

;; ********************************************************************
;; Exercise 13.9, p. 441
 
(display-results "13.9")

(require "evaluate.scm")  ; evaluate.scm requires Ex13.9.scm

;; The following should cause the stack to be enlarged twice,
;; first from 8 cells to 16 and then from 16 to 32.
;; Have your push! procedure display the message "Enlarging stack ..."
;; when it needs to double the size of the vector.

(assert (= (evaluate "(1+(2+(3+(4+(5+(6+(7+(8+9))))))))") 45)) 


;; ********************************************************************          
;; Allows repetitive queue operations for the following queue tests

(define from-to-do  
  (lambda (start stop body)
    (if (> start stop)
        (values)
        (begin (body start)
               (from-to-do (+ 1 start) stop body)))))


;; ********************************************************************          
;; Exercise 13.11, p. 451

(display-results "13.11")

(require "Ex13.11.scm")  ; the two-vector circular queue implementation

(define queue (make-queue))

;; First we add 1, 2, 3, 4, 5, 6 to the queue
;; Then we dequeue 4 times

(from-to-do 1 6 (lambda (n) (enqueue! queue n))) ;; from-to-do returns nothing
(from-to-do 1 4 (lambda (n) (dequeue! queue)))
(assert (= (head queue) 5)) ;; should not cause an error

;; Now we add 11, 12, 13, 14, 15, 16 to the queue
;; We should have wrapped around so the vector length is still 8
;; Then we dequeue 5 times

(from-to-do 11 16 (lambda (n) (enqueue! queue n)))
(assert (= (vector-length (queue-cells queue)) 8))  ;; should not cause an error
(from-to-do 1 5 (lambda (n) (dequeue! queue)))
(assert (= (head queue) 14)) ;; should not cause an error

;; Dequeue 3 more times and queue should be empty

(from-to-do 1 3 (lambda (n) (dequeue! queue)))
(assert (empty-queue? queue))


;; ********************************************************************
;; Exercise 13.13, p. 453

(display-results "13.13")

(require "Ex13.13.scm")  ; the linked-node queue implementation

(set! queue (make-queue))

;; First we add 1, 2, 3, 4, 5, 6 to the queue
;; Then we dequeue 4 times

(from-to-do 1 6 (lambda (n) (enqueue! queue n))) ;; from-to-do returns nothing
(from-to-do 1 4 (lambda (n) (dequeue! queue)))
(assert (= (head queue) 5)) ;; should not cause an error

;; Now we add 11, 12, 13, 14, 15, 16 to the queue
;; Then we dequeue 5 times

(from-to-do 11 16 (lambda (n) (enqueue! queue n)))
(from-to-do 1 5 (lambda (n) (dequeue! queue)))
(assert (= (head queue) 14)) ;; should not cause an error

;; Dequeue 3 more times and queue should be empty

(from-to-do 1 3 (lambda (n) (dequeue! queue)))
(assert (empty-queue? queue)) ;; should not cause an error

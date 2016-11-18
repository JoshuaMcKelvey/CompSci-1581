;; Joshua McKelvey
;; Lab Exercise 10 -- Text Exercise 13.13
;; CS 1581 Honors Computer Science I
;; Fall 2016

(module Ex13.13 racket

  (provide make-queue
           empty-queue?
	   head
	   enqueue!
	   dequeue!)

  ;; ********************************************************************
  ;; Exercise 13.13, p. 453

  (define make-queue
    (lambda ()
      (let ((queue (mcons '() '())))
        (set-start! queue '())
        (set-tail! queue '() )
        queue)
        ))
  
  (define empty-queue?
    (lambda (queue)
      (null? (start queue)))
      )

  (define head
    (lambda (queue)
      (if (empty-queue? queue)
          '()
          (mcar (start queue))
          )))

  (define enqueue!
    (lambda (queue new-item)
      (define add-item
    (lambda (list item)
      (if (null? list)
          (mcons item '())
          (mcons (mcar list) (add-item (mcdr list) item)))))
      (if (empty-queue? queue)
          (let ((head (start queue))
                (tail (tail queue))
                (new-node (make-node new-item)))
            (set-start! queue new-node)
            (set-tail! queue new-node)
            queue)
          (begin
            (set-start! queue (add-item (start queue) new-item))
            (set-tail! queue (make-node new-item))
             queue)
      )))

  (define dequeue!
    (lambda (queue)
      (if (empty-queue? queue)
          (error "queue is empty")
          (if (null? (start queue))
              (begin
                (set-tail! queue '())
                queue)
            (begin
            (set-start! queue (mcdr (start queue)))
            queue)))))


  ;; Any supporting non-ADT operations here:

  (define start      ;accessor for start of queue
    (lambda (queue)
      (mcar queue)))
  
  (define set-start! ;mutator for start of queue
    (lambda (queue node)
      (set-mcar! queue node)))
  
  (define tail       ;accessor for tail of queue
    (lambda (queue)
      (mcdr queue)))
  (define make-node
    (lambda (node)
       (define mnode (mcons node '()))
      mnode
      ))
  (define set-tail!  ;mutator for tail of queue
    (lambda (queue node)
      (set-mcdr! queue node)))
  
)
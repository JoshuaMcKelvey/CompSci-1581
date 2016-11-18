;; Joshua McKelvey
;; Lab Exercise 10 -- Text Exercise 13.9
;; CS 1581 Honors Computer Science I
;; Fall 2016

(module Ex13.9 racket

  (provide make-ra-stack
           height
	   empty-ra-stack?
	   top-minus
	   pop!
	   push!)

  ;; ********************************************************
  ;; Advertised procedures -- these make up the ADT interface

  (define make-ra-stack
    (lambda ()
      (make-ra-stack-with-at-most 8))) 

  (define height
    (lambda (ra-stack)
      (vector-ref ra-stack 0)))

  (define empty-ra-stack?
    (lambda (ra-stack)
      (= 0 (height ra-stack))))

  (define top-minus
    (lambda (ra-stack offset)
      (cond ((< offset 0)
	     (error "TOP-MINUS: offset < 0" offset))
	    ((>= offset (height ra-stack))
	     (error "TOP-MINUS: offset too large for stack"
		    offset (height ra-stack)))
	    (else
	     (vector-ref (cells ra-stack)
			 (- (height ra-stack)
			    (+ offset 1)))))))

  (define pop!
    (lambda (ra-stack)
      (if (empty-ra-stack? ra-stack)
	  (error "POP!: attempted pop from an empty stack")
	  (begin
	    (set-height! ra-stack
			 (- (height ra-stack) 1))
	    ra-stack))))

  ;; ********************************************************************
  ;; Exercise 13.9, p. 441
 
  (define push! 
    (lambda (ra-stack item)
      (if (equal?  (vector-length (cells ra-stack)) (height ra-stack))
          (begin
            (display "Expanding Stack ...")
            (newline)
            (let ((cells (cells ra-stack))
                  (new-vector (make-vector
                               (* (height ra-stack) 2))))
              (vector-copy! new-vector
                            0
                            cells
                            0
                            (height ra-stack))
              (set-cells! ra-stack new-vector)
              (push! ra-stack item)))
          (let ((new-cell (cells ra-stack)))
            (vector-set! new-cell (height ra-stack) item)
            (set-height! ra-stack (+ (height ra-stack) 1))
            (set-cells! ra-stack new-cell)
            ra-stack)
          )
          
          ))
  

  
  ;; ********************************************************
  ;; Unadvertised procedures -- these are for implementors only

  (define make-ra-stack-with-at-most
    (lambda (max-height)
      (let ((header (make-vector 2))
	    (cells (make-vector max-height)))
	(vector-set! header 0 0)     ; header[0] = height = 0
	(vector-set! header 1 cells) ; header[1] = cells
	header)))

  (define cells  ; use only within the ADT implementation
    (lambda (ra-stack)
      (vector-ref ra-stack 1)))

  (define set-height!  ; use only within the ADT implementation
    (lambda (ra-stack new-height)
      (vector-set! ra-stack 0 new-height)))

  (define set-cells!  ; use only within the ADT implementation
    (lambda (ra-stack new-cells)
      (vector-set! ra-stack 1 new-cells)))

  (define v 
        (let ((ra-stack (make-ra-stack))
              (cells '#(1 2 3 4 5 6 7 9)))
          (set-height! ra-stack 8)
          (set-cells! ra-stack cells)
          ra-stack)
        )
(define fuckyea (lambda (ra-stack item) (vector-set! (cells ra-stack) (height ra-stack) item)))
)
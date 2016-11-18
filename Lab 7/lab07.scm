;; Joshua McKelvey
;; Lab Exercise 7
;; CS 1581 Honors Computer Science I
;; Fall 2016

(module lab07 racket

  (provide integers-from-to
	   count
	   count-satisfying
           my-list-ref
           position)

  ;; **************************************************************
  ;; Exercise 7.4, p. 172

  (define integers-from-to ;; Must be iterative
    (lambda (low high)
    (define iter
      (lambda (high lst)
        (if (> low high)
            lst
            (iter (- high 1)
                  (cons high lst)))))
    (iter high '())))

  ;; **************************************************************
  ;; Exercise 7.6, p. 173

  ;; Part a
  (define count            ; counts number of times item occurs
    (lambda (item lst)     ; in lst
      (define count-int
        (lambda (item lst count)
          (cond ((null? lst) count)
                ((equal? item (car lst)) (count-int item (cdr lst) (+ count 1)))
                (else (count-int item (cdr lst) count)))
             ))
      (count-int item lst 0)
      ))

  ;; Part b
  (define count-satisfying ; counts number of items occur in lst
    (lambda (pred? lst)    ; that satisfy pred?
      (define count-int
        (lambda (lst count)
          (cond ((null? lst) count)
                ((pred? (car lst)) (count-int (cdr lst) (+ count 1)))
                (else (count-int (cdr lst) count)))
             ))
      (count-int lst 0)))

  ;; **************************************************************
  ;; Exercise 7.7, p. 174

  (define my-list-ref
    (lambda (lst n)
      (define count-int
        (lambda (lst n count)
          (cond ((null? lst) '!!!ERROR!!!)
                ((equal? count n) (car lst))
                (else (count-int (cdr lst) n (+ count 1))))
             ))
      (if (>= n 0)
          (count-int lst n 0)
          '!!!ERROR!!!)
      ))

  ;; **************************************************************
  ;; Exercise 7.8e, p. 174
  
  (define position
    (lambda (element lst)
      (define count-int
        (lambda (element lst count)
          (cond ((null? lst) -1)
                ((equal? element (car lst)) (+ count 1))
                (else (count-int element (cdr lst) (+ count 1))))
     ))
      (count-int element lst -1)
      ))

)
;; <Your name here>
;; Joshua McKelvey
;; Lab Exercise 5
;; CS 1581 Honors Computer Science I
;; Fall 2016

(module lab05 racket

  (provide sum-func
	   make-exponentiator
	   make-series
	   make-verifier
	   f-upc)


  ;; ****************************************************************************
  ;; Exercise 5.6, p. 113

  (define sum-func
    (lambda (low high f)

      (define sum-sub-func
        (lambda (low high f value)
             (if (= low high)
                 (+ value (f high))
                 (sum-sub-func (+ low 1) high f (+ value (f low))
                ))))
      (sum-sub-func low high f 0)
      ))


  ;; ****************************************************************************
  ;; Exercise 5.7, p. 119

  (define make-exponentiator
    (lambda (e)
      (lambda (x) (expt x e))))


  ;; ****************************************************************************
  ;; Exercise 5.8, p. 120

  (define make-series
    (lambda (f)
      (lambda (g)
      (define func-fact
        (lambda (func n)
          (if (= n 1)
              1
              (func (func-fact func (- n 1)) n))))
      (func-fact f g)
      )))
      
  ;; ****************************************************************************
  ;; Exercise 5.11, p. 121
(require (lib "trace.ss"))

  (define make-verifier
    (lambda (f m)
      (lambda (n)
        (define verif-mod
          (lambda (func mast count org digit)
          (if (= count 12)
              (if (integer? (/ digit mast))
                  #t
                  #f)
              (verif-mod func mast (+ count 1) (quotient org 10) (+ digit (func count (modulo org 10))))
              )))
        
        (verif-mod f m 1 n 0))))

  ;; ****************************************************************************
  ;; Exercise 5.12, p. 122

  (define f-upc 
    (lambda (i di)
      (if (odd? i)
          di
          (* 3 di)
          )))

)
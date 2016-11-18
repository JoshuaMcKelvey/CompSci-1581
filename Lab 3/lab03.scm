;; Joshua McKelvey 
;; Lab Exercise 3
;; CS 1581 Honors Computer Science I
;; Fall 2016

(module lab03 racket

  (provide sum-of-divisors-eff
	   renumber
	   survives?
	   first-survivor-after
	   divides?)

  (define divides?   ; Required for Ex. 3.6
    (lambda (a b)
      (= (remainder b a) 0)))

  ;; **************************************************************
  ;; Exercise 3.6, p. 60

  (define sum-of-divisors-eff 
    (lambda (n) 
  (define divides?
     (lambda (a b)
       (= (remainder b a) 0)))
  (define sum-from-plus ; sum of all divisors of n which are
    (lambda (low addend) ; >= low, plus addend
      (if (and (> low n) (>= low (sqrt n)))
          addend ; no divisors of n are greater than n
          (sum-from-plus (+ low 1)
                         (if (divides? low n)
                             (+ addend low)
                             addend)))))
  (sum-from-plus 1 0)))

  ;; **************************************************************
  ;; Exercise 3.10, p. 68                                     

  (define renumber
    (lambda (position n)
          (if (<= position 2)
              (+ position (- n 3))
              (- position 3))))
              
          

  ;; **************************************************************
  ;; Exercise 3.11, p. 68                                            

  (define survives?
    (lambda (position n)
      (if (and (< n 3) (>= position n))
          #t
          (if (< position 3)
              #t
              (if (= position 3)
                  #f
                  (survives? (- position 3) n) )))))

  ;; **************************************************************
  ;; Exercise 3.12, p. 68                                                           

  (define first-survivor-after
    (lambda (position n)
      (if (<= n 2) ;verifying that the n is not less than 3
          (renumber position  n) ;if n is less than 3, then renumber finding next number
          (if (= position 0) ; if position is 0, then will renumber position and test again
              (first-survivor-after (renumber position (- n 1)) (- n 1))
              (if (> position 0); if position is greater than 0, will test again finding next number
                  (first-survivor-after (renumber (+ position 1) n) (- n 1))
                  (first-survivor-after 0 n)
      ))))) 
)
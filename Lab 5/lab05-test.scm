(require "lab05.scm")
(require "test-utilities.scm")

;; ****************************************************************************
;; Exercise 5.6, p. 113
(display-results "5.6")

(assert (= (sum-func 5 10 (lambda (n) (* n n))) 355))

(assert (= (sum-func 10 100 sqrt) 652.156946577112))


;; ****************************************************************************
;; Exercise 5.7, p. 119
(display-results "5.7")

(define square (make-exponentiator 2))

(assert (= (square 4) 16))

(define cube (make-exponentiator 3))

(assert (= (cube 4) 64))


;; ****************************************************************************
;; Exercise 5.8, p. 120
(display-results "5.8")

(define factorial (make-series *))

(assert (= (factorial 5) 120))

(define sum-of-first (make-series +))

(assert (= (sum-of-first 5) 15))


;; ****************************************************************************
;; Exercise 5.11, p. 121
(display-results "5.11")

(define check-isbn (make-verifier * 11))

(assert (equal? (check-isbn 0262010771) #T))

(assert (equal? (check-isbn 0262010770) #F))


;; ****************************************************************************
;; Exercise 5.12, p. 122
(display-results "5.12")

(define check-upc (make-verifier f-upc 10))

(assert (equal? (check-upc 038000145001) #T))

(assert (equal? (check-upc 038000154001) #F))

(assert (equal? (check-upc 044300107250) #T))

(assert (equal? (check-upc 044300107350) #F))

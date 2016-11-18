(require "lab07.scm")
(require "test-utilities.scm")

;; **************************************************************
;; Exercise 7.4, p. 172

(display-results "7.4")

(assert (equal? (integers-from-to 2 7) '(2 3 4 5 6 7)))

(assert (equal? (integers-from-to 25 37) '(25 26 27 28 29 30 31 32 33 34 35 36 37)))

;; **************************************************************
;; Exercise 7.6a, p. 173

(display-results "7.6a")

(assert (= (count 3 '(2 4 6 8 10)) 0))

(assert (= (count 3 '(3 2 3 4 5 4 3 2 3)) 4))

(assert (= (count 'the '(the quick brown fox jumped over the lazy dog the)) 3))

;; **************************************************************
;; Exercise 7.6b, p. 173

(display-results "7.6b")

(assert (= (count-satisfying odd? '(1 2 3 4 5 6 7 8 9)) 5))

(assert (= (count-satisfying (lambda (n) (= (remainder n 2) 1)) '(1 2 3 4 5 6 7 8 9)) 5))

;; **************************************************************
;; Exercise 7.7, p. 174

(display-results "7.7")

(assert (equal? (my-list-ref '(are we having fun yet) 0) 
                'are))

(assert (equal? (my-list-ref '(are we having fun yet) 4) 
                'yet))

(assert (equal? (my-list-ref '(are we having fun yet) -1) 
                '!!!ERROR!!!))

(assert (equal? (my-list-ref '(are we having fun yet) 5) 
                '!!!ERROR!!!))

;; **************************************************************
;; Exercise 7.8e, p. 174

(display-results "7.8e")

(assert (= (position 'are '(are we having fun yet))
           0))

(assert (= (position 'having '(are we having fun yet))
           2))

(assert (= (position 'yet '(are we having fun yet))
           4))

(assert (= (position 'foo '(are we having fun yet))
           -1))
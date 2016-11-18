(require "lab02.scm")
(require "lab01.scm") ; To use rcross-bb and my-bb
(require "test-utilities.scm")

;; ********************************************************************
;; Tests for Ex. 2.1, page 28
 
(display-results "2.1")

(assert (= (power 3 0) 1))  ; should not cause error

(assert (= (power 3 1) 3))

(assert (= (power 3 2) 9))

(assert (= (power 4 4) 256))

(assert (= (power -4 4) 256))

(assert (= (power -4 3) -64))


;; ********************************************************************
;; Tests for Ex. 2.5, page 36
 
(display-results "2.5")

(assert (= (multiply 3 4) 12))

(assert (= (multiply 100 0) 0))

(assert (= (multiply 0 100) 0))

(assert (= (multiply 123 456) 56088))

(assert (= (multiply -123 456) -56088))

(assert (= (multiply 123 -456) -56088))

(assert (= (multiply -123 -456) 56088))


;; ********************************************************************
;; Tests for Ex. 2.9a, page 39
 
(display-results "2.9a")

(assert (= (num-sixes 0) 0))

(assert (= (num-sixes 606060606) 5))

(assert (= (num-sixes 123457890) 0))

(assert (= (num-sixes 61234567890) 2))

(assert (= (num-sixes 6666) 4))

(assert (= (num-sixes -6666) 4))


;; ********************************************************************
;; Tests for Ex. 2.9b, page 39
 
(display-results "2.9b")

(assert (= (num-digits 0 0) 1))

(assert (= (num-digits 606060606 6) 5))

;(assert (= (num-digits 606060606 0) 4))

(assert (= (num-digits 606060606 5) 0))

;(assert (= (num-digits 61234567890 0) 1))

(assert (= (num-digits 61234567890 6) 2))

(assert (= (num-digits -61234567890 6) 2))


;; ********************************************************************
;; Tests for Ex. 2.13, page 40
 
(display-results "2.13")

(display "Should be a stack of five rcross-bb basic blocks:\n")

(stack-copies-of 5 rcross-bb)

(display "Should be a stack of three my-bb basic blocks:\n")

(stack-copies-of 3 my-bb)


;; ********************************************************************
;; Tests for Ex. 2.14, page 40

(display-results "2.14")

(display "Should be a 4 x 3 pattern of crosses:\n")

(quilt (pinwheel rcross-bb) 4 3)

(display "Should be a 3 x 5 pattern of your quilt:\n")

(quilt (pinwheel my-bb) 3 5)

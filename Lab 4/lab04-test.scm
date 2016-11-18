(require "lab04.scm")
(require "test-utilities.scm")

;; ********************************************************************
;; Tests for Ex. 4.8, page 60.  Do as many calls to my-curve as are practical

(display-results "4.8: Do as many of these as are practical")
 
(my-curve 0 -1/2 0 1/2 0)  ;; This should give you the vertical line
(my-curve 0 -1/2 0 1/2 1)
(my-curve 0 -1/2 0 1/2 2)
(my-curve 0 -1/2 0 1/2 3)
(my-curve 0 -1/2 0 1/2 4)
(my-curve 0 -1/2 0 1/2 5)
(my-curve 0 -1/2 0 1/2 6)  ;; You should be able to get through at least this one

(my-curve -1/2 0 1/2 0 6)  ;; This should be the same as the previous but with
                           ;; a different orientation

;; The rest may be too complex depending on how you define my-curve
;(my-curve 0 -1/2 0 1/2 7)  
;(my-curve 0 -1/2 0 1/2 8) 
;(my-curve 0 -1/2 0 1/2 9)
;(my-curve 0 -1/2 0 1/2 10)

;; ********************************************************************
;; Exercise 4.9

(display-results "4.9: Should produce big equilateral triangle")
 
(triangle -1 -.75 1 -.75 0 1)

;; ********************************************************************
;; Exercise 4.10

(display-results "4.10: These should look like the triangles in fig. 4.8, p. 96")
 
(sierpinskis-gasket -1 -.75 1 -.75 0 1 0)
(sierpinskis-gasket -1 -.75 1 -.75 0 1 1)
(sierpinskis-gasket -1 -.75 1 -.75 0 1 2)

(display-results "4.10: This should produce fig. 4.6, p. 95")
 
(sierpinskis-gasket -1 -.75 1 -.75 0 1 6)  
(require "lab01.scm")    ;; Load all your definitions
(require "test-utilities.scm")
(require (lib "fungraph.ss" "concabs"))

;; **************************************************************
;; Some procedures that produce test images

(define test-bb
  (filled-triangle 0 1 0 -1 1 -1))

(define nova-bb
  (overlay (filled-triangle 0 1 0 0 -1/2 0)
           (filled-triangle 0 0 0 1/2 1 0)))


;; **************************************************************
;; Exercise 1.6

(display-results "1.6")

(assert (= (turkey-servings 10) 40/3))
(assert (= (turkey-servings 15) 30))


;; **************************************************************
;; Exercise 1.7

(display-results "1.7")

(display "\nYour answer to problem 1.7a: \n")
1.7a                 ;; should say what puzzle1 does

(display "\nYour answer to problem 1.7b:\n")
1.7b                 ;; should say what puzzle2 does


;; **************************************************************
;; Exercise 1.9

(display-results "1.9")

(display "\n(half-turn test-bb) should show a triangle pointing down: \n")
(half-turn test-bb)  

(display "\n(quarter-turn-left test-bb) should show a triangle pointing left: \n")
(quarter-turn-left test-bb) 

(display "\n(side-by-side test-bb (half-turn test-bb)) should show a triangle pointing up to the left of a triangle pointing down: \n")
(side-by-side test-bb (half-turn test-bb)) 
                            
;; **************************************************************
;; Exercise 1.10

(display-results "1.10")


(display "\n(pinwheel test-bb) should behave as shown on page 17 of the text: \n")
(pinwheel test-bb)

(display "\n(pinwheel nova-bb) should show a tilted four-pointed star: \n")
(pinwheel nova-bb) 

;; **************************************************************
;; Exercise 1.11

(display-results "1.11")

(display "\ncorner-bb should look like its counterpart in figure 1.4 on page 16: \n")
corner-bb 

(display "\nrcross-bb should look like its counterpart in figure 1.4 on page 16: \n")
rcross-bb 

(display "\nmy-bb should show your invented basic block: \n")
my-bb     

(display "\n(pinwheel (pinwheel (pinwheel rcross-bb))) should show a large version of the repeating crosses pattern shown on page 15: \n")
(pinwheel (pinwheel (pinwheel rcross-bb)))

(display "\n(pinwheel (pinwheel (pinwheel my-bb))) should show what sort of quilt pattern your basic block generates.:\n")
(pinwheel (pinwheel (pinwheel my-bb)))

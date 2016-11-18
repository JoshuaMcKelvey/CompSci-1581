;; Joshua McKelvey
;; Lab Exercise 1
;; CS 1581 Honors Computer Science I
;; Fall 2016

(module lab01 racket

  (provide turkey-servings
	   1.7a
	   1.7b
	   half-turn
	   quarter-turn-left
	   side-by-side
	   pinwheel
	   corner-bb
	   rcross-bb
	   my-bb)

  (require (lib "fungraph.ss" "concabs"))

;; **************************************************************
;; Textbook Exercise 1.6

  (define turkey-servings (lambda (turkey-size) (if (<= turkey-size 12)
                                                    (/ turkey-size 3/4)
                                                    (/ turkey-size 1/2))))

;; **************************************************************
;; Textbook Exercise 1.7

  (define 1.7a "puzzle1 computes the addition of value a to the greatest value of b or c." )

  (define 1.7b "puzzle2 computes the absolute value of an integer." )

;; **************************************************************
;; Textbook Exercise 1.9

(define half-turn
    (lambda (x)
     (quarter-turn-right
      (quarter-turn-right x))))

(define quarter-turn-left
    (lambda (x)
     (quarter-turn-right
      (quarter-turn-right
       (quarter-turn-right x)))))

  (define side-by-side 
   (lambda (x y)
     (quarter-turn-right
      (stack
       (quarter-turn-left x)
       (quarter-turn-left y)))))

;; **************************************************************
;; Textbook Exercise 1.10

  (define pinwheel 
    (lambda (x)
     (stack 
     (quarter-turn-right
      (stack
       (quarter-turn-right x)
        x))
     (quarter-turn-right
      (stack
       (half-turn x)
       (quarter-turn-left x))))))

;; **************************************************************
;; Textbook Exercise 1.11

  (define corner-bb 
    (filled-triangle 1 1 0 1 1 0) )

  (define rcross-bb 
   (overlay
    (overlay
    (quarter-turn-left
     (filled-triangle 0.5 0.5 -0.5 -0.5 -0.5 0.5))
    (overlay (filled-triangle -1 1 1 1 -0.5 0.5)
             (filled-triangle 1 0.5 1 1 -0.5 0.5)
            ))
    (overlay
     (filled-triangle 1 -1 1 0.5 0.5 -0.5)
     (filled-triangle 0.5 0.5 1 0.5 0.5 -0.5))))

  (define my-bb 
    (overlay
     (filled-triangle 0 -1 0 0 1 -1)
     (filled-triangle 0 1 0 0 -1 1)))
)
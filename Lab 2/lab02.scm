;; Joshua McKelvey
;; Lab Exercise 2
;; CS 1581 Honors Computer Science I
;; Fall 2016

(module lab02 racket

  (provide power
	   multiply
	   num-sixes
	   num-digits
	   stack-copies-of
	   quilt)

  (require "lab01.scm")
  (require (lib "fungraph.ss" "concabs"))

  ;; **************************************************************
  ;; Solution for Ex. 2.1, page 28

  (define power
  (lambda (base exp)
    (if (= exp 0)
        1
        (* (power (+ base 0) (- exp 1)) base))))


  ;; **************************************************************
  ;; Solution for Ex. 2.5, page 36

  (define multiply
  (lambda (m n)
   (if (= m 0)
       0
       (if (= n 0)
           0
           (if (< n 0)
               (- (multiply m (- n)))
               (if (< m 0)
                   (- (multiply (- m) n))
                   (if (= n 1)
                       m
                       (+ m (multiply m (- n 1))
           ))))))))


  ;; **************************************************************
  ;; Solution for Ex. 2.9a, page 39

  (define num-sixes         ; number of 6's in decimal rep of n
    (lambda (n)
      (if (< n 0)
          (num-sixes (- n))
          (if (= n 0)
              0
              (if (= 6 (remainder n 10))
                  (+ 1 (num-sixes (quotient n 10)))
                  (num-sixes (quotient n 10)))))))


  ;; **************************************************************
  ;; Solution for Ex. 2.9b, page 39

  (define num-digits         ; number of d's in decimal rep of n
    (lambda (n d)
      (if (and (= n 0) (= d 0))
               1
               (if (< n 0)
                   (num-digits (- n) d)
                   (if (= n 0)
                       0
                       (if (= d (modulo n 10))
                           (+ 1 (num-digits (quotient n 10) d))
                           (num-digits (quotient n 10) d)))))))
      

  ;; **************************************************************
  ;; Solution for Ex. 2.13, page 40

  (define stack-copies-of       ; stack n copies of image on top
    (lambda (n image)           ; one another
      (if (= n 1)        ;stack until n = 1
          image       ; when n = 1 return image
          (stack (stack-copies-of (- n 1) image) image)))) ;Loop will recursively return stacked images


  ;; **************************************************************
  ;; Solution for Ex. 2.14, page 40

  (define quilt             ; make a pattern which is w images wide
    (lambda (image w h)     ; and h images high
      (if (= h 1)
          image
          (stack (side-by-side (stack-copies-of (- h 1) image) w) (side-by-side image w))) ;Loop will recursively insert stacked images
      ))

(define side-by-side
  (lambda (image w)
    (if (= w 1)
        image ;stack until n = 1
        (quarter-turn-right ;loop will run until w = 1 then return images
           (stack
              (quarter-turn-left image)
              (quarter-turn-left (side-by-side image (- w 1))))))))
)

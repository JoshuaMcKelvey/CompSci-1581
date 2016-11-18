(define power
  (lambda (base exp)
    (if (= exp 0)
        1
        (* (power (+ base 0) (- exp 1)) base))))

(require (lib "fungraph.ss" "concabs"))
(require (lib "trace.ss"))
(define multiply
  (lambda (m n)
    (if (= n 1)
        m
        (+ m (multiply m (- n 1))
           ))))

(define corner-bb 
    (filled-triangle 1 1 0 1 1 0) )

(define quot
      (lambda (n d)
        (if (< d 0)
            (- (quot n (- d)))
            (if (< n 0)
                (- (quot (- n) d))
                (if (< n d)
                    0
                    (+ 1 (quot (- n d) d)))))))


(define stack-copies-of       ; stack n copies of image on top
    (lambda (n image)           ; one another
      (if (= n 1)        ;stack until n = 1
          image       ; when n = 1 return image
          (stack (stack-copies-of (- n 1) image) image)))) ;Loop will recursively insert stacked images

(define half-turn
    (lambda (x)
     (quarter-turn-right
      (quarter-turn-right x))))

(define quarter-turn-left
    (lambda (x)
     (quarter-turn-right
      (quarter-turn-right
       (quarter-turn-right x)))))

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
(trace quilt)
(trace power)

(define num-digits-six         ; number of d's in decimal rep of n
    (lambda (n)
      (if (<= n 0)
        0
       (num-digits-six (/ n 10))
      )))
(trace num-digits-six)
(define count-digits
  (lambda (n); 1
  (if (< n 10)                     ; 2
      1                                       ; 3
      (+ 1 (count-digits (/ n 10)))))) ; 4

(define num-sixes         ; number of 6's in decimal rep of n
    (lambda (n)
      (if (< n 0)
          (num-sixes (- n))
          (if (= n 0)
              0
              (if (= 6 (remainder n 10))
                  (+ 1 (num-sixes (quotient n 10)))
                  (num-sixes (quotient n 10)))))))


(define count-digits
  (lambda (n)
  (if (< n 10)                    
      1                                      
      (+ 1 (count-digits (/ n 10))))))
  (define num-digits         ; number of d's in decimal rep of n
    (lambda (n d)
      (if (and (= n 0) (= d 0))
               1
               (if (< n 0)
                   (num-digits (- n) d)
                   (if (and (= n 0) (<= (count-digits d) 0))
                       0
                       (if (= d (modulo n 10))
                           (+ 1 (num-digits (quotient n 10) d))
                           (num-digits (quotient n 10) d)))))))

 (trace num-digits)
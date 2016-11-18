(require "lab06.scm")
(require "test-utilities.scm")

;; **************************************************************
;; Preliminary Exercise for 6.22: ADT for an interval 

(display-results "6.22 (Preliminary): Interval Constructor and Accessors")

(assert (= (lower-endpoint (make-interval 3 7)) 3))
(assert (= (upper-endpoint (make-interval 3 7)) 7))

;; **************************************************************
;; Exercise 6.22, p. 161

(display-results "6.22")

(assert (= (midpoint (make-interval 3 7)) 5))

(assert (let ((i (right-half (make-interval 3 7))))
          (and (= (lower-endpoint i) 5)
               (= (upper-endpoint i) 7))))

(assert (= (midpoint (right-half (make-interval 3 7))) 6))

;; **************************************************************
;; Preliminary Exercise for 6.23: ADT for a 3D Vector 

(display-results "6.23 (Preliminary): 3D Vector Constructor and Accessors")

(assert (= (x-coord (make-3D-vector 1 2 3)) 1))
(assert (= (y-coord (make-3D-vector 1 2 3)) 2))
(assert (= (z-coord (make-3D-vector 1 2 3)) 3))

(display-results "Preliminary Exercise: 3D Vector Equality")

(assert (3D-vector-equals (make-3D-vector 1 2 3) 
                          (make-3D-vector 1 2 3)))

(assert (not (3D-vector-equals (make-3D-vector 1 2 3) 
                               (make-3D-vector 0 2 3))))

(assert (not (3D-vector-equals (make-3D-vector 1 2 3) 
                               (make-3D-vector 1 0 3))))

(assert (not (3D-vector-equals (make-3D-vector 1 2 3) 
                               (make-3D-vector 1 2 0))))

;; **************************************************************
;; Exercise 6.23, p. 162

(display-results "6.23")

(assert (3D-vector-equals (3D-vector-add (make-3D-vector 1 2 3) 
                                         (make-3D-vector 4 5 6)) 
                          (make-3D-vector 5 7 9)))

(assert (= (3D-vector-dot-product (make-3D-vector 1 2 3) 
                                  (make-3D-vector 4 5 6))
           32))

(assert (3D-vector-equals (3D-vector-scale (make-3D-vector 1 2 3) 17)
                          (make-3D-vector 17 34 51)))
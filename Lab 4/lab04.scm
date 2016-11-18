;; <Your name here>
;; Lab Exercise 4
;; CS 1581 Honors Computer Science I
;; <Semester and year here, e.g. Fall 2012>

(module lab04 racket

  (provide my-curve
	   triangle
	   sierpinskis-gasket)

  (require (lib "fungraph.ss" "concabs"))

  (set-default-image-size! 300)

  ;; *****************************************************************
  ;; Exercise 4.8, p. 99

  (define my-curve
  (lambda (x0 y0 x1 y1 level)
    (if (= level 0)
        (line x0 y0 x1 y1)

        (let ((xmid (/ (+ x0 x1) 2))
              (ymid (/ (+ y0 y1) 2))
              (dx (- x1 x0))
              (dy (- y1 y0)))
          (let ((xa (- xmid (/ dy 2)))
                (ya (+ ymid (/ dx 2))))

            (overlay (my-curve x0 y0 xa ya (- level 1))
                     (overlay
                      (my-curve x0 y1 x1 y1 (- level 1))
                      (overlay
                       (my-curve xa ya x1 ymid (- level 1))
                       (my-curve xmid ya x0 y1 (- level 1))
                       ))))))))
  ; *****************************************************************
  ;; Exercise 4.9, p. 99

  (define triangle
    (lambda (x0 y0 x1 y1 x2 y2)
      (overlay (line x0 y0 x1 y1)
               (overlay (line x1 y1 x2 y2)
                        (line x2 y2 x0 y0)))))

  ; *****************************************************************
  ;; Exercise 4.10, p. 99
;(define quarter-turn-left
;    (lambda (x)
;     (quarter-turn-right
;      (quarter-turn-right
;       (quarter-turn-right x)))))
;  (define side-by-side
;  (lambda (image w)
;    (if (= w 1)
;        image ;stack until n = 1
;        (quarter-turn-right ;loop will run until w = 1 then return images
;           (stack
;              (quarter-turn-left image)
;              (quarter-turn-left (side-by-side image (- w 1))))))))
  ;
  (define square
    (lambda (x)
      (* x x)))
  (define distance-formula
    (lambda (x0 y0 x1 y1)
      (sqrt (+ (square (- x1 x0)) (square (- y1 y0))))))
  (define sierpinskis-gasket
    (lambda (x0 y0 x1 y1 x2 y2 level)
      (if (= level 0)
          (triangle x0 y0 x1 y1 x2 y2)
          (let
              ((midx0 (/ (+ x0 x1) 2))
               (midx1 (/ (+ x1 x2) 2))
               (midx2 (/ (+ x2 x0) 2))
               (midy0 (/ (+ y0 y1) 2))
               (midy1 (/ (+ y1 y2) 2))
               (midy2 (/ (+ y2 y0) 2)))
              
          (overlay (sierpinskis-gasket midx2 midy2 midx1 midy1 x2 y2 (- level 1))
                   (overlay
                    (sierpinskis-gasket x0 y0 midx0 midy0 midx2 midy2 (- level 1))
                    (sierpinskis-gasket midx0 midy0 x1 y1 midx1 midy1 (- level 1))
          ))))))

)
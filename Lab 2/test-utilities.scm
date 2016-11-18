;; This module provides utilities to aid in the testing of 
;; lab exercise procedures

(module test-utilities racket
  
  (require racket/pretty)
  (pretty-print-columns 50)
  
  (provide display-results
           assert)
  
  ;; To help display test results in a readable way
  (define display-results  
    (lambda (exercise)
      (newline)
      (display
       "**************************************************************")
      (newline)
      (display "Results for Exercise ")
      (display exercise)
      (newline)
      (newline)))
  
  ;; This defines an assert macro which allows a test to use, e.g.
  ;;      (assert (= x y))
  ;; rather than
  ;;      (assert "(= x y)") or (assert '(= x y))
  (define-syntax-rule (assert condition) 
    (do-assert 'condition))              
  
  (define do-assert 
    (lambda (condition)
      (let ((out (open-output-string)))
        (pretty-display condition out)
        (let ((condition-string (get-output-string out)))
          (if (not (eval condition))
              (error (format "Assertion failed:\n~a\n" condition-string))
              (display (format "Assertion OK:\n~a\n" condition-string))))))))
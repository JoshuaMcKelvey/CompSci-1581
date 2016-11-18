(require "lab09.scm")
(require "bstree.scm")
(require "test-utilities.scm")

(define tree-1 '(9 (6 (5 () ()) ()) (18 (11 () (13 () (17 () ()))) (65 (52 (41 (39 () ()) ()) ()) (99 () ())))))

(define tree-2 '(9 (8 (7 (6 (5 (4 (3 (2 (1 () ()) ()) ()) ()) ()) ()) ()) ()) ()))

(define tree-3 '(9 (8 (7 (6 (5 (4 (3 (2 (1 () ()) ()) ()) ()) ()) ()) ()) ()) 
                   (10 () (11 () (12 () (13 () (14 () (15 () (16 () (17 () ()))))))))))


;; **************************************************************
;; Exercise 8.1, p. 217

(display-results "8.1")

(assert (= (minimum tree-1) 5))

(assert (= (minimum tree-2) 1))


;; **************************************************************
;; Exercise 8.2, p. 217

(display-results "8.2")

(assert (= (number-of-nodes tree-1) 12))

(assert (= (number-of-nodes tree-2) 9))


;; **************************************************************
;; Exercise 8.4, p. 220

(display-results "8.4")

(assert (equal? (inorder tree-1) '(5 6 9 11 13 17 18 39 41 52 65 99)))

(assert (equal? (inorder tree-2) '(1 2 3 4 5 6 7 8 9)))


;; **************************************************************
;; Exercise 8.5, p. 220

(display-results "8.5")

(assert (equal? (postorder tree-1) '(5 6 17 13 11 39 41 52 99 65 18 9)))

(assert (equal? (postorder tree-2) '(1 2 3 4 5 6 7 8 9)))


;; Exercise 8.6, p. 220

(display-results "8.6")

(assert 
 (equal? (insert 5 
	   (insert 17 
	     (insert 39 
	       (insert 6 
		 (insert 13 
	           (insert 41 
		     (insert 99 
		       (insert 52 
			 (insert 11 
		  	   (insert 65 
			     (insert 18 
			       (insert 9 (make-empty-tree)))))))))))))
	 tree-1))

(assert 
 (equal? (insert 17 
           (insert 16 
             (insert 15 
               (insert 14 
                 (insert 13 
                   (insert 12 
                     (insert 11 
                       (insert 10 tree-2))))))))
	 tree-3)) 


;; **************************************************************
;; Exercise 8.7, p. 220

(display-results "8.7")

(assert (equal? (list->bstree '(5 17 39 6 13 41 99 52 11 65 18 9)) tree-1))

(assert (equal? (list->bstree '(1 2 3 4 5 6 7 8 9)) tree-2))


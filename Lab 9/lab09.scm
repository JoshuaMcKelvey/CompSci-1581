;; Joshua McKelvey
;; Lab Exercise 9
;; CS 1581 Honors Computer Science I
;; Fall 2016

(module lab09 racket

  (provide minimum
	   number-of-nodes
	   inorder
	   postorder
	   insert
	   list->bstree)

  (require "bstree.scm")
  (require racket/trace)
  (define tree-1 '(9 (6 (5 () ()) ()) (18 (11 () (13 () (17 () ()))) (65 (52 (41 (39 () ()) ()) ()) (99 () ())))))
  
  (define tree-2 '(9 (8 (7 (6 (5 (4 (3 (2 (1 () ()) ()) ()) ()) ()) ()) ()) ()) ()))
  
  (define tree-3 '(9 (8 (7 (6 (5 (4 (3 (2 (1 () ()) ()) ()) ()) ()) ()) ()) ()) 
                     (10 () (11 () (12 () (13 () (14 () (15 () (16 () (17 () ()))))))))))

  ;; **************************************************************
  ;; Exercise 8.1, p. 217

  (define minimum      
    (lambda (tree)
      (define minimum-sub
        (lambda (elt tree)
          (if (empty-tree? tree)
              elt
              (minimum-sub (root tree) (left-subtree tree)))))
      (minimum-sub (root tree) tree)
      ))

  ;; **************************************************************
  ;; Exercise 8.2, p. 217

  (define number-of-nodes
    (lambda (tree)
      (define subsub
        (lambda (tree)
        (if (empty-tree? tree)
          1
          (+ (number-of-nodes (right-subtree tree))
             (number-of-nodes (left-subtree tree))
             ))))
       (subsub tree)
       ))
(trace number-of-nodes)

  ;; **************************************************************
  ;; Exercise 8.4, p. 220

  (define inorder
       (lambda (tree)
         (define combi
           (lambda (lst1 lst2)
             (if (empty? lst1)
                 lst2
                 (cons (car lst1) (combi (cdr lst1) lst2)))))
         (if (empty-tree? tree)
             '()
             (combi (inorder (left-subtree tree))
                     (cons (root tree) (inorder (right-subtree tree)))))))
;
;   (define inorder-2
;       (lambda (tree list)
;        (cond ((empty-tree? tree) list)
;              ((not (empty-tree? (left-subtree tree))) (

  ;; **************************************************************
  ;; Exercise 8.5, p. 220

  (define postorder
    (lambda (tree list)
      (cond ((empty-tree? tree) list)
            ((empty-tree? tree) (postorder (cdr tree) (cons (car tree) list))
            ((empty-tree? (left-subtree tree)) (postorder tree list))
      ))))


  ;; **************************************************************
  ;; Exercise 8.6, p. 220

  (define insert
    (lambda (n tree)   
      (cond ((empty-tree? tree) (make-leaf n))
            ((< n (root tree))
             (make-nonempty-tree (root tree) (insert n (left-subtree tree)) (right-subtree tree)))
            ((> n (root tree))
             (make-nonempty-tree (root tree) (left-subtree tree) (insert n (right-subtree tree)))))))


  ;; **************************************************************
  ;; Exercise 8.7, p. 220

  (define list->bstree
    (lambda (lst)
      (define list-sub
        (lambda (lst1 lst2)
          (if (null? lst1)
              lst2
              (list-sub (cdr lst1) (cons (car lst1) lst2)))))
      (list-sub '(lst) '())
      (trace list-sub)
      ))
  (trace list->bstree)
(define list-sub
        (lambda (lst1 lst2)
          (if (null? lst1)
              lst2
              (list-sub (cdr lst1) (insert (car lst1) lst2)))))
)
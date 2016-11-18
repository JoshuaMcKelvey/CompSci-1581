;; This file contains excerpts from the textbook Concrete
;; Abstractions: An Introduction to Computer Science Using Scheme, by
;; Max Hailperin, Barbara Kaiser, and Karl Knight, Copyright (c) 1998
;; by the authors. Full text is available for free at
;; http://www.gustavus.edu/+max/concrete-abstractions.html

;; Chapter 8: Trees

;; 8.1  Binary Search Trees

(module bstree racket

  (provide make-empty-tree
	   make-nonempty-tree
	   empty-tree?
	   root
	   left-subtree
	   right-subtree
	   make-leaf)

  (define make-empty-tree
    (lambda () '()))

  (define make-nonempty-tree
    (lambda (root left-subtree right-subtree)
      (list root left-subtree right-subtree)))

  (define empty-tree? null?)

  (define root car)

  (define left-subtree cadr)

  (define right-subtree caddr)

  (define make-leaf ; added by tcolburn
    (lambda (root)
      (make-nonempty-tree root (make-empty-tree) (make-empty-tree))))

  )
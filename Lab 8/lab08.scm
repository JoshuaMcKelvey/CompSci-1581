;; Joshua McKelvey
;; Lab Exercise 8
;; CS 1581 Honors Computer Science I
;; Fall 2016

(module lab08 racket

  (provide movies-satisfying
	   director-is-actor?
	   the-only-element-in
	   actors-action
	   all-movies-for-actor-action
	   substitutions-in-to-match
	   matches?)

  (require "movie-database.scm")
  (require "movie-adts.scm")

  ;; **************************************************************
  ;; Exercise 7.24, p. 190

  
  
  (define movies-satisfying          
    (lambda (movies pred selector)
      (define filter
       (lambda (ok? lst)
         (cond ((null? lst) '())
               ((ok? (car lst)) (cons (selector (car lst))
                                      (filter ok? (cdr lst))))
               (else (filter ok? (cdr lst))))))
      (filter pred movies)))

  ;; An added problem for 7.24
  (define director-is-actor?
    (lambda (movie)
     (define selector
      (lambda (lst1 lst2)
         (cond ((null? lst2) #f)
               ((equal? lst1 (car lst2)) #t)
               (else (selector lst1 (cdr lst2))))))
      (selector (movie-director movie) (movie-actors movie))))


  ;; **************************************************************
  ;; Exercise 7.26, p. 197
(require racket/trace)
  (define substitutions-in-to-match
    (lambda (pattern query)
      (cond ((null? query) '())
            ((equal? (cdr pattern) '(...)) (cons (cdr query) '()))
            ((equal? (car pattern) (car query)) (substitutions-in-to-match (cdr pattern) (cdr query)))
            )))
(trace substitutions-in-to-match)
  ;; **************************************************************
  ;; Exercise 7.27, p. 197

  ;; You don't have to write any code for this exercise, just run the
  ;; test file.


  ;; **************************************************************
  ;; Exercise 7.28, p. 197

  (define the-only-element-in
    (lambda (lst)
      (car lst)))


  ;; **************************************************************
  ;; Exercise 7.25, p. 196

  (define actors-action
    (lambda (title)
      (the-only-element-in (movies-satisfying 
       our-movie-database
       (lambda (movie) (equal? (movie-title movie) title))
       movie-actors))
      ))
(require racket/trace)
  (define all-movies-for-actor-action
    (lambda (actor)
     (define selector
      (lambda (lst1 lst2)
         (cond ((null? lst2) #f)
               ((equal? lst1 (car lst2)) #t)
               (else (selector lst1 (cdr lst2))))))
      (movies-satisfying our-movie-database
                         (lambda (movie)
                           (selector actor (movie-actors movie)))  movie-title)))
  
  (define movie (make-movie '(amarcord)
		      '(federico fellini)
		      1974
		      '((magali noel) (bruno zanin)
			(pupella maggio)
			(armando drancia))))
(trace all-movies-for-actor-action)
  ;; **************************************************************
  ;; matches? is provided to run the tests

  (define matches?
    (lambda (pattern question)
      (cond ((null? pattern)  (null? question))
	    ((null? question) #f)
	    ((list? (car pattern))
	     (if (member (car question) (car pattern))
		 (matches? (cdr pattern)
			   (cdr question))
		 #f))
	    ((equal? (car pattern) '...) #t)
	    ((equal? (car pattern) (car question))
	     (matches? (cdr pattern)
		       (cdr question)))
	    (else #f))))

)
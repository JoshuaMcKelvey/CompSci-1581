;; Joshua McKelvey
;; Assignment 4
;; CS 1581 Honors Computer Science I
;; Fall 2016

(module assignment4 racket

  (provide movies-satisfying
	   director-is-actor?
	   the-only-element-in

	   director-action
	   actors-action
	   all-movies-for-actor-action
	   made-in-year-action
	   movies-before-after-action
	   movies-between-action
	   when-made-action
	   appear-in-between-action
	   direct-between-action
	   word-in-title-action
	   director-and-star-action
	   two-stars-action

	   matches?
	   substitutions-in-to-match

	   redefine-actors-action-for-7.33

	   redefine-matches?-for-7.29
	   redefine-substitutions-in-to-match-for-7.30
	   redefine-matches?-for-7.34
	   redefine-substitutions-in-to-match-for-7.34
	   redefine-matches?-for-7.37
	   redefine-substitutions-in-to-match-for-7.37
	   redefine-substitutions-in-to-match-for-7.35)

  (require "movie-database.scm")
  (require "movie-adts.scm")

;; **************************************************************
;; Preliminary: Copy your procedures from Lab 8
(require racket/trace)
  (define movies-satisfying          
    (lambda (movies pred selector)
      (define filter
       (lambda (ok? lst)
         (cond ((null? lst) '())
               ((ok? (car lst)) (cons (selector (car lst))
                                      (filter ok? (cdr lst))))
               (else (filter ok? (cdr lst))))))
      ;(trace filter)
      (filter pred movies)))
  ;(trace movies-satisfying)
  (define director-is-actor?
    (lambda (movie)
     (define selector
      (lambda (lst1 lst2)
         (cond ((null? lst2) #f)
               ((equal? lst1 (car lst2)) #t)
               (else (selector lst1 (cdr lst2))))))
      (selector (movie-director movie) (movie-actors movie))))

  (define substitutions-in-to-match
    (lambda (pattern query)
      (cond ((null? query) '())
            ((equal? (cdr pattern) '(...)) (cons (cdr query) '()))
            ((equal? (car pattern) (car query)) (substitutions-in-to-match (cdr pattern) (cdr query)))
            )))
  (define the-only-element-in
    (lambda (lst)
      (car lst)))

  (define actors-action
    (lambda (title)
      (the-only-element-in (movies-satisfying 
       our-movie-database
       (lambda (movie) (equal? (movie-title movie) title))
       movie-actors))
      ))
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

;; ************************************************************************
;; You will be enhancing this matches? predicate which is provided by the text.

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

;; ************************************************************************
;; You will also be enhancing other procedures defined here.
;; To redefine an already defined name use the "redefine" macro
;; defined here:

  (define-syntax-rule (redefine symbol value)
    (set! symbol value))

;; Example: if foo has already been defined as a procedure, use
;;
;;    (redefine foo (lambda ... ))
;;
;; to redefine it.  This is necessary because modules do not allow 
;; the "define" form to be used more than once on a name.

;; ************************************************************************
;; exercise 7.29

  (define redefine-matches?-for-7.29
    (lambda ()
      (redefine matches?
                (lambda (pattern question)
                  (define selector
                  (lambda (lst1 lst2)
                    (cond ((null? lst2) #f)
                          ((equal? lst1 (car lst2)) #t)
                          (else (selector lst1 (cdr lst2))))))
                  (cond ((null? pattern)  (null? question))
                        ((null? question) #f)
                        ((list? (car pattern))
                         (if (selector (car question) (car pattern))
                             (matches? (cdr pattern)
                                       (cdr question))
                             #f))
                        ((equal? (car pattern) '...) #t)
                        ((equal? (car pattern) '_)
                         (if (equal? (cdr question) '())
                             #t
                             (matches? (cdr pattern)
                                       (cdr question))))
                        ((equal? (car pattern) (car question))
                         (matches? (cdr pattern)
                                   (cdr question)))
                        (else #f))))
      ;(trace matches?)
      ))

;(trace matches?)
  ;; ************************************************************************
  ;; exercise 7.30    

  (define redefine-substitutions-in-to-match-for-7.30
    (lambda ()
      (redefine substitutions-in-to-match     
                (lambda (pattern query)
                  (define selector
                    (lambda (lst1 lst2)
                      (cond ((null? lst2) #f)
                            ((equal? lst1 (car lst2)) #t)
                            (else (selector lst1 (cdr lst2))))))
                  (cond ((null? query) '())
                        ((equal? (car pattern) '...)
                         (cons query (substitutions-in-to-match pattern '())))
                        ((and (equal? (car pattern) '_) (equal? (cdr pattern) '()))
                         (cons query (substitutions-in-to-match pattern '())))
                        ((equal? (car pattern) '_)
                         (cons (car query) (substitutions-in-to-match
                                            (cdr pattern) (cdr query))))
                        ((equal? (car pattern) (car query))
                         (substitutions-in-to-match (cdr pattern) (cdr query)))
                        ((list? (car pattern))
                         (if (selector (car query) (car pattern))
                             (cons (car query) (substitutions-in-to-match (cdr pattern)
                                                                          (cdr query)))
                             
                             #f))
                        )))
      ;(trace substitutions-in-to-match )
      )
    )
      
      

  (define made-in-year-action
    (lambda (noun verb year)
     (movies-satisfying 
       our-movie-database
       (lambda (movie) (if (list? year)
                           (equal? (movie-year-made movie) (the-only-element-in year))
                           (equal? (movie-year-made movie) year)))
       movie-title)
      ))

  ;; ************************************************************************
  ;; Exercise 7.31

  (define movies-before-after-action 
    (lambda (noun verb preposition year)
      (cond ((equal? preposition 'in)
             (movies-satisfying 
              our-movie-database
              (lambda (movie) (equal? (movie-year-made movie) (the-only-element-in year)))
              movie-title))
            ((equal? preposition 'before)
             (movies-satisfying 
              our-movie-database
             (lambda (movie) (< (movie-year-made movie) (the-only-element-in year)))
             movie-title))
            ((equal? preposition 'after)
             (movies-satisfying 
              our-movie-database
             (lambda (movie) (> (movie-year-made movie) (the-only-element-in year)))
             movie-title))
            ((equal? preposition 'since)
             (movies-satisfying 
              our-movie-database
             (lambda (movie) (>= (movie-year-made movie) (the-only-element-in year)))
             movie-title)))))

  ;; ************************************************************************
  ;; Exercise 7.32

  (define movies-between-action 
    (lambda (noun verb low high)
      (movies-satisfying 
       our-movie-database
      (lambda (movie) (and
                       (>= (movie-year-made movie) low)
                       (<= (movie-year-made movie) (the-only-element-in high))))
       movie-title)))


  ;; ************************************************************************
  ;; Exercise 7.33

  (define director-action  ; an enhancement of the movie director action
    (lambda (title)        ; given on p. 197 of the text
      (define filter
       (lambda (lst)
         (cond ((null? lst) '())
               ((or (equal? (car lst) 'a)
                    (equal? (car lst) 'the)
                    (equal? (car lst) 'an)) (filter (cdr lst)))
               (else (cons  (car lst)
                                      (filter (cdr lst)))))))
      (the-only-element-in
       (movies-satisfying
        our-movie-database
        (lambda (movie) (equal? (filter (movie-title movie)) (filter title)))
        movie-director))))



  (define redefine-actors-action-for-7.33
    (lambda ()
      (redefine actors-action  ; modified for ex. 7.33
	 (lambda (title)
           (define filter
             (lambda (lst)
               (cond ((null? lst) '())
                     ((or (equal? (car lst) 'a)
                          (equal? (car lst) 'the)
                          (equal? (car lst) 'an)) (filter (cdr lst)))
                     (else (cons  (car lst)
                                  (filter (cdr lst)))))))
           (the-only-element-in
            (movies-satisfying
             our-movie-database
             (lambda (movie) (equal? (filter (movie-title movie)) (filter title)))
             movie-actors))))))


  ;; ************************************************************************
  ;; Exercise 7.34

  (define redefine-matches?-for-7.34
    (lambda ()
      (redefine matches?
                (lambda (pattern question)
                  (define selector
                    (lambda (lst1 lst2)
                      (cond ((null? lst2) #f)
                            ((equal? lst1 (car lst2)) #t)
                            (else (selector lst1 (cdr lst2))))))
                  (cond ((and (null? pattern)  (null? question)) #t)
                        ((null? question) #f)
                        ((equal? pattern '(... made)) #t)
                        ((and (equal? (car pattern) '...) (equal? (car (cdr pattern)) 'appear))
                               (matches? (cdr pattern) (filter-appear-direct-in question)))
                        ((and (equal? (car pattern) '...) (equal? (car (cdr pattern)) 'direct))
                               (matches? (cdr pattern) (filter-appear-direct-in question)))
                        ((equal? (car pattern) '...)
                          #t)
                        ((and (equal? (car pattern) '_) (equal? (cdr pattern) '()))
                         #t)
                        ((list? (car pattern))
                         (if (selector (car question) (car pattern))
                             (matches? (cdr pattern)
                                       (cdr question))
                             #f))
                        ((equal? (car pattern) '_)
                         (if (equal? (cdr question) '())
                             #t
                             (matches? (cdr pattern)
                                       (cdr question))))
                        ((equal? (car pattern) (car question))
                         (matches? (cdr pattern)
                                   (cdr question)))
                        (else #f))))
      ;(trace matches?)
      ))

  
(define filter-made
             (lambda (lst)
               (cond ((null? lst) '())
                     ((equal? (car lst) 'made)
                      (filter-made (cdr lst)))
                     (else (cons  (car lst)
                                  (filter-made (cdr lst)))))))
  (define filter-appear-direct
             (lambda (lst)
               (cond ((null? lst) '())
                     ((equal? (car lst) 'direct)
                      '())
                     ((equal? (car lst) 'appear)
                      '())
                     (else (cons  (car lst)
                                  (filter-appear-direct (cdr lst)))))))
  (define filter-appear-direct-in
             (lambda (lst)
               (cond ((null? lst) '())
                     ((equal? (car lst) 'direct)
                      lst)
                     ((equal? (car lst) 'appear)
                      lst)
                     (else (filter-appear-direct-in (cdr lst))))))
  (define redefine-substitutions-in-to-match-for-7.34
    (lambda ()
      (redefine substitutions-in-to-match     
           (lambda (pattern query)
                  (define selector
                    (lambda (lst1 lst2)
                      (cond ((null? lst2) #f)
                            ((equal? lst1 (car lst2)) #t)
                            (else (selector lst1 (cdr lst2))))))
                  (cond ((null? query) '())
                        ((equal? pattern '(... made))
                         (cons (filter-made query) (substitutions-in-to-match pattern '())))
                        ((and (equal? (car pattern) '...) (equal? (car (cdr pattern)) 'appear))
                         (cons (filter-appear-direct query)
                               (substitutions-in-to-match (cdr pattern) (filter-appear-direct-in query))))
                         ((and (equal? (car pattern) '...) (equal? (car (cdr pattern)) 'direct))
                         (cons (filter-appear-direct query)
                               (substitutions-in-to-match (cdr pattern) (filter-appear-direct-in query))))
                        ((equal? (car pattern) '...)
                         (cons query (substitutions-in-to-match pattern '())))
                        ((and (equal? (car pattern) '_) (equal? (cdr pattern) '()))
                         (cons query (substitutions-in-to-match pattern '())))
                        ((equal? (car pattern) '_)
                         (cons (car query) (substitutions-in-to-match
                                            (cdr pattern) (cdr query))))
                        ((equal? (car pattern) (car query))
                         (substitutions-in-to-match (cdr pattern) (cdr query)))
                        ((list? (car pattern))
                         (if (selector (car query) (car pattern))
                             (cons (car query) (substitutions-in-to-match (cdr pattern)
                                                                          (cdr query)))
                             
                             #f))
                        )))
      ;(trace substitutions-in-to-match)
      ))
  
  (define when-made-action
    (lambda (title)
      (define filter
             (lambda (lst)
               (cond ((null? lst) '())
                     ((or (equal? (car lst) 'a)
                          (equal? (car lst) 'the)
                          (equal? (car lst) 'an)) (filter (cdr lst)))
                     (else (cons  (car lst)
                                  (filter (cdr lst)))))))
      (the-only-element-in (movies-satisfying
             our-movie-database
             (lambda (movie) (equal? (filter (movie-title movie)) (filter title)))
             movie-year-made))))


  ;; ************************************************************************
  ;; Exercise 7.36

  (define appear-in-between-action
    (lambda (noun verb actor low high)
      (define selector
        (lambda (lst1 lst2)
          (cond ((null? lst2) #f)
                ((equal? lst1 (car lst2)) #t)
                (else (selector lst1 (cdr lst2))))))
      (movies-satisfying our-movie-database
                         (lambda (movie)
                           (and 
                            (selector actor (movie-actors movie))
                            (>= (movie-year-made movie) low)
                            (<= (movie-year-made movie) (the-only-element-in high)))
                           )
      movie-title)))
  
  (define direct-between-action
    (lambda (noun verb director low high)
      (movies-satisfying our-movie-database
                         (lambda (movie)
                           (and      (equal? director (movie-director movie))
                                     (>= (movie-year-made movie) low)
                                     (<= (movie-year-made movie) (the-only-element-in high)))
                           ) movie-title)))


  ;; ************************************************************************
  ;; Exercise 7.37

  (define redefine-matches?-for-7.37
    (lambda ()
      (redefine matches?                   
                (lambda (pattern question)
                  (define selector
                    (lambda (lst1 lst2)
                      (cond ((null? lst2) #f)
                            ((equal? lst1 (car lst2)) #t)
                            (else (selector lst1 (cdr lst2))))))
                  (cond ((and (null? pattern)  (null? question)) #t)
                        ((null? question) #f)
                        ((equal? pattern '(... made)) #t)
                        ((and (equal? (car pattern) '...) (equal? (car (cdr pattern)) 'appear))
                         (matches? (cdr pattern) (filter-appear-direct-in question)))
                        ((and (equal? (car pattern) '...) (equal? (car (cdr pattern)) 'direct))
                         (matches? (cdr pattern) (filter-appear-direct-in question)))
                        ((and (equal? (car pattern) '...) (equal? (car (cdr pattern)) 'star))
                         (matches? (cdr pattern) (filter-final-in question)))
                        ((and (equal? (car pattern) '...) (equal? (car (cdr pattern)) 'and))
                         (matches? (cdr pattern) (filter-final-in question)))
                        ((equal? (car pattern) '...)
                         #t)
                        ((and (equal? (car pattern) '_) (equal? (cdr pattern) '()))
                         #t)
                        ((list? (car pattern))
                         (if (selector (car question) (car pattern))
                             (matches? (cdr pattern)
                                       (cdr question))
                             #f))
                        ((equal? (car pattern) '_)
                         (if (equal? (cdr question) '())
                             #t
                             (matches? (cdr pattern)
                                       (cdr question))))
                        ((equal? (car pattern) (car question))
                         (matches? (cdr pattern)
                                   (cdr question)))
                        ((equal? (car pattern) 'number?)
                         (if (number? (car question))
                             (matches? (cdr pattern)
                                       (cdr question))
                             #f))
                        ((equal? (car pattern) 'symbol?)
                         (if (symbol? (car question))
                             (matches? (cdr pattern)
                                       (cdr question))
                             #f))
                             (else #f))
                  ))
      ;(trace matches?)
      ))

  (define redefine-substitutions-in-to-match-for-7.37
    (lambda ()
      (redefine substitutions-in-to-match     
	(lambda (pattern query)
                  (define selector
                    (lambda (lst1 lst2)
                      (cond ((null? lst2) #f)
                            ((equal? lst1 (car lst2)) #t)
                            (else (selector lst1 (cdr lst2))))))
                  (cond ((null? query) '())
                        ((equal? pattern '(... made))
                         (cons (filter-made query) (substitutions-in-to-match pattern '())))
                        ((and (equal? (car pattern) '...) (equal? (car (cdr pattern)) 'appear))
                         (cons (filter-appear-direct query)
                               (substitutions-in-to-match (cdr pattern) (filter-appear-direct-in query))))
                         ((and (equal? (car pattern) '...) (equal? (car (cdr pattern)) 'direct))
                         (cons (filter-appear-direct query)
                               (substitutions-in-to-match (cdr pattern) (filter-appear-direct-in query))))
                        ((equal? (car pattern) '...)
                         (cons query (substitutions-in-to-match pattern '())))
                        ((and (equal? (car pattern) '_) (equal? (cdr pattern) '()))
                         (cons query (substitutions-in-to-match pattern '())))
                        ((equal? (car pattern) '_)
                         (cons (car query) (substitutions-in-to-match
                                            (cdr pattern) (cdr query))))
                        ((equal? (car pattern) (car query))
                         (substitutions-in-to-match (cdr pattern) (cdr query)))
                        ((equal? (car pattern) 'number?)
                         (if (number? (car query))
                             (cons (car query) (substitutions-in-to-match (cdr pattern)
                                       (cdr query)))
                             #f))
                        ((equal? (car pattern) 'symbol?)
                         (if (symbol? (car query))
                             (cons (car query) (substitutions-in-to-match (cdr pattern)
                                       (cdr query)))
                             #f))
                        ((list? (car pattern))
                         (if (selector (car query) (car pattern))
                             (cons (car query) (substitutions-in-to-match (cdr pattern)
                                                                          (cdr query)))
                             
                             #f)))))
      ;(trace substitutions-in-to-match)
      ))

  (define word-in-title-action
    (lambda (noun verb word)
     (define selector
      (lambda (lst1 lst2)
         (cond ((null? lst2) #f)
               ((equal? lst1 (car lst2)) #t)
               (else (selector lst1 (cdr lst2))))))
      (movies-satisfying our-movie-database
                         (lambda (movie)
                           (selector word (movie-title movie)))  movie-title)))


  ;; ************************************************************************
  ;; Exercise 7.35
  (define filter-final
    (lambda (lst)
      (cond ((null? lst) '())
            ((equal? (car lst) 'direct)
             '())
            ((equal? (car lst) 'star)
             '())
            ((equal? (car lst) 'and)
             '())
            ((equal? (car lst) 'appear)
             '())
            (else (cons  (car lst)
                         (filter-final (cdr lst)))))))
  ;(trace filter-final)
  (define filter-final-in
             (lambda (lst)
               (cond ((null? lst) '())
                     ((equal? (car lst) 'direct)
                      lst)
                     ((equal? (car lst) 'star)
                      lst)
                     ((equal? (car lst) 'and)
                      lst)
                     ((equal? (car lst) 'appear)
                      lst)
                     (else (filter-final-in (cdr lst))))))
  (define redefine-substitutions-in-to-match-for-7.35
    (lambda ()
      (redefine substitutions-in-to-match
             (lambda (pattern query)
               (define substitutions-in-to-match-sub
                (lambda (pattern query)
                  (define selector
                    (lambda (lst1 lst2)
                      (cond ((null? lst2) #f)
                            ((equal? lst1 (car lst2)) #t)
                            (else (selector lst1 (cdr lst2))))))
                  (cond ((null? query) '())
                        ((equal? pattern '(... made))
                         (cons (filter-made query) (substitutions-in-to-match-sub pattern '())))
                        ((and (equal? (car pattern) '...) (equal? (cdr pattern) '()))
                         (cons query (substitutions-in-to-match-sub pattern '())))
                        ((and (equal? (car pattern) '...) (equal? (car (cdr pattern)) 'and))
                         (cons (filter-final query)
                               (substitutions-in-to-match-sub (cdr pattern) (filter-final-in query))))
                        ((and (equal? (car pattern) '...) (equal? (car (cdr pattern)) 'star))
                         (cons (filter-final query)
                               (substitutions-in-to-match-sub (cdr pattern) (filter-final-in query))))
                        ((and (equal? (car pattern) '...) (equal? (car (cdr pattern)) 'appear))
                         (cons (filter-final query)
                               (substitutions-in-to-match-sub (cdr pattern) (filter-final-in query))))
                        ((and (equal? (car pattern) '...) (equal? (car (cdr pattern)) 'direct))
                         (cons (filter-final query)
                               (substitutions-in-to-match-sub (cdr pattern) (filter-final-in query))))
                        ((and (equal? (car pattern) '_) (equal? (cdr pattern) '()))
                         (cons query (substitutions-in-to-match-sub pattern '())))
                        ((equal? (car pattern) '_)
                         (cons (car query) (substitutions-in-to-match-sub
                                            (cdr pattern) (cdr query))))
                        ((equal? (car pattern) (car query))
                         (substitutions-in-to-match-sub (cdr pattern) (cdr query)))
                        ((equal? (car pattern) 'number?)
                         (if (number? (car query))
                             (cons (car query) (substitutions-in-to-match-sub (cdr pattern)
                                                                          (cdr query)))
                             #f))
                        ((equal? (car pattern) 'symbol?)
                         (if (symbol? (car query))
                             (cons (car query) (substitutions-in-to-match-sub (cdr pattern)
                                                                          (cdr query)))
                             #f))
                        ((list? (car pattern))
                         (if (selector (car query) (car pattern))
                             (cons (car query) (substitutions-in-to-match-sub (cdr pattern)
                                                                          (cdr query)))
                             
                             #f))
                        (else '()))))
               ;(trace substitutions-in-to-match-sub)
               (let ((x (substitutions-in-to-match-sub pattern query)))
               (if (equal? x '(movies))
                   '()
                  x))
     ))
      ;(trace substitutions-in-to-match)
      ))

  (define director-and-star-action
    (lambda (noun director star)
      (define selector
      (lambda (lst1 lst2)
         (cond ((null? lst2) #f)
               ((equal? lst1 (car lst2)) #t)
               (else (selector lst1 (cdr lst2))))))
      (movies-satisfying our-movie-database
                         (lambda (movie)
                           (and (selector star (movie-actors movie)) (equal? director (movie-director movie))))
                         movie-title)))

  (define two-stars-action
    (lambda (noun star1 star2)
     (define selector
      (lambda (lst1 lst2)
         (cond ((null? lst2) #f)
               ((equal? lst1 (car lst2)) #t)
               (else (selector lst1 (cdr lst2))))))
      (movies-satisfying our-movie-database
                         (lambda (movie)
                           (and (selector star1 (movie-actors movie)) (selector star2 (movie-actors movie))))
                         movie-title)))

)
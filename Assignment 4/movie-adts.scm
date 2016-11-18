;; This module defines ADTs for movies and pattern/action pairs

(module movie-adts racket

  (provide make-movie           ; movie constructor
	   movie-title          ; movie accessors
	   movie-director
	   movie-year-made
	   movie-actors

	   make-pattern/action  ; pattern/action constructor
	   pattern              ; pattern/action accessors
	   action

	   movie-p/a-list       ; pattern/action list and mutators
	   add-to-p/a-list
	   clear-p/a-list)

  (define make-movie
    (lambda (title director year-made actors)
      (list title director year-made actors)))

  (define movie-title car)
  (define movie-director cadr)
  (define movie-year-made caddr)
  (define movie-actors cadddr)

  (define make-pattern/action
    (lambda (pattern action)
      (cons pattern action)))

  (define pattern car)
  (define action cdr)

  (define movie-p/a-list '())

  (define add-to-p/a-list
    (lambda (pattern action)
      (set! movie-p/a-list 
	    (cons (make-pattern/action pattern action) movie-p/a-list))))

  (define clear-p/a-list
    (lambda ()
      (set! movie-p/a-list '())))

)
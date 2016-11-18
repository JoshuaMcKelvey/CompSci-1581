(require "lab08.scm")
(require "movie-database.scm")
(require "movie-adts.scm")
(require "test-utilities.scm")
(require "movie-test-utilities.scm")

(clear-p/a-list)

;; **************************************************************
;; Exercise 7.24, p. 190

(display-results "7.24")    ; movies-satisfying and director-is-actor?
			    ; provided by lab08.scm
(assert (equal? (movies-satisfying our-movie-database
				   (lambda (movie)
				     (= (movie-year-made movie) 1974))
				   movie-title)
		'((amarcord) (chinatown))))

(assert (equal? (movies-satisfying our-movie-database
				   director-is-actor?
				   movie-title)
		'((dead again) (citizen kane) (othello) (chinatown) (malcolm x))))

(assert (equal? (movies-satisfying our-movie-database
				   director-is-actor?
				   movie-director)
		'((kenneth branagh) (orson welles) (orson welles) (roman polanski) (spike lee))))


;; **************************************************************
;; Exercise 7.26, p. 197

(display-results "7.26")   ; substitutions-in-to-match provided by lab08.scm

(assert (equal? (substitutions-in-to-match '(foo bar baz)
					   '(foo bar baz))
		'()))

(assert (equal? (substitutions-in-to-match '(foo ...)
					   '(foo bar))
		'((bar))))

(assert (equal? (substitutions-in-to-match '(foo ...)
					   '(foo bar baz)) 
		'((bar baz))))


;; **************************************************************
;; Exercise 7.27, p. 197

(display-results "7.27")

(add-to-p/a-list 
 '(who is the director of ...)
 (lambda (title)
   (movies-satisfying 
    our-movie-database
    (lambda (movie) (equal? (movie-title movie) title))
    movie-director)))

(let ((queries '((who is the director of amarcord)
		 (who is the director of a fish called wanda)))
      (results '(((federico fellini))
		 ((michael chrichton)))))
  (query-test queries results))

;; **************************************************************
;; Exercise 7.28, p. 197

(display-results "7.28")

(define director-action
  (lambda (title)
    (the-only-element-in   ; the-only-element-in provided by lab08.scm
     (movies-satisfying 
      our-movie-database
      (lambda (movie) (equal? (movie-title movie) title))
      movie-director))))

(add-to-p/a-list
 '(who is the director of ...)
 director-action)

(let ((queries '((who is the director of amarcord)
		 (who is the director of a fish called wanda)))
      (results '((federico fellini)
		 (michael chrichton))))
  (query-test queries results))


;; **************************************************************
;; Exercise 7.25, p. 196

(display-results "7.25")

(add-to-p/a-list
 '(who acted in ...)
 actors-action)    ; actors-action provided in lab08.scm

(add-to-p/a-list
 '(what movies have starred ...)
 all-movies-for-actor-action) ; all-movies-for-actor-action provided in lab08.scm

(let ((queries '((who acted in johnny got his gun)
		 (who acted in metropolis)
		 (what movies have starred alec guinness)
		 (what movies have starred orson welles)))
      (results '(((timothy bottoms) (kathy fields) (jason robards) (diane varsi) 
		  (donald sutherland) (eduard franz))
		 ((alfred abel) (gustay frohlich) (brigitte helm) (rudolf kleinrogge) 
		  (heinrich george))
		 ((lawrence of arabia))
		 ((citizen kane) (othello)))))
  (query-test queries results))

;; **************************************************************
;; Uncomment the following if you would like to run the interactive
;; query loop following completion of the automated tests

;; (query-loop)
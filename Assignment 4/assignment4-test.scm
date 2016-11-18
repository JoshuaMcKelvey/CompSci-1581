(require "assignment4.scm")
(require "movie-database.scm")
(require "movie-adts.scm")
(require "test-utilities.scm")
(require "movie-test-utilities.scm")

(clear-p/a-list)

;; **************************************************************
;; Preliminary: Do the final test from Lab 8

;(display-results "Final Lab 8 exercise (7.25)")
;
;(add-to-p/a-list
; '(who acted in ...)
; actors-action)    ; actors-action provided in Lab 8
;
;(add-to-p/a-list
; '(what movies have starred ...)
; all-movies-for-actor-action) ; all-movies-for-actor-action provided in Lab 8
;
;(query-test '((who acted in johnny got his gun)
;	      (who acted in metropolis)
;	      (what movies have starred alec guinness)
;	      (what movies have starred orson welles))
;	    '(((timothy bottoms) (kathy fields) (jason robards) (diane varsi) 
;	       (donald sutherland) (eduard franz))
;	      ((alfred abel) (gustay frohlich) (brigitte helm) (rudolf kleinrogge) 
;	       (heinrich george))
;	      ((lawrence of arabia))
;	      ((citizen kane) (othello))))

;; ************************************************************************
;; Exercises 7.29 and 7.30    

(display-results "7.29 and 7.30")

(redefine-matches?-for-7.29)
(redefine-substitutions-in-to-match-for-7.30)

(add-to-p/a-list
 '(who acted in ...)
 actors-action)    ; actors-action provided in Lab 8

(add-to-p/a-list
 '(what movies have starred ...)
 all-movies-for-actor-action) ; all-movies-for-actor-action provided in Lab 8

(query-test '((who acted in johnny got his gun)
	      (who acted in metropolis)
	      (what movies have starred alec guinness)
	      (what movies have starred orson welles))
	    '(((timothy bottoms) (kathy fields) (jason robards) (diane varsi) 
	       (donald sutherland) (eduard franz))
	      ((alfred abel) (gustay frohlich) (brigitte helm) (rudolf kleinrogge) 
	       (heinrich george))
	      ((lawrence of arabia))
	      ((citizen kane) (othello))))

(add-to-p/a-list  ; new for exercise 7.29 and 7.30
 '(what (movie movies) (was were) made in _)
 made-in-year-action)

(query-test '((what movies were made in 1952)
	      (what movie was made in 1974)
	      (what movies was made in 1991)) 
	    '(((an american in paris) (othello))
	      ((amarcord) (chinatown))
	      ((boyz n the hood) (dead again))))

;; ************************************************************************
;; Exercise 7.31

(display-results "7.31")

(add-to-p/a-list  ; new for exercise 7.31
 '(what (movie movies) (was were) made (in before after since) _)
 movies-before-after-action)

(query-test '((what movies were made before 1952)
	      (what movies were made in 1952)
	      (what movies were made after 1952)
	      (what movies were made since 1991))
	    '(((casablanca) (citizen kane) (gone with the wind) (metropolis))
	      ((an american in paris) (othello))
	      ((amarcord) (the big easy) (boyz n the hood) (dead again) (the godfather) 
	       (lawrence of arabia) (the manchurian candidate) (spartacus) 
	       (a star is born) (after the rehearsal) (amadeus) (blood simple) 
	       (chinatown) (the cotton club) (the crying game) (the day of the jackal) 
	       (diva) (the dresser) (el norte) (the exorcist) (a fish called wanda) 
	       (flirting) (gates of heaven) (house of games) (iceman) (jaws) 
	       (johnny got his gun) (local hero) (malcolm x))
	      ((boyz n the hood) (dead again) (the crying game) (flirting) (malcolm x))))
;
;;; ************************************************************************
;;; Exercise 7.32
;
(display-results "7.32")

(add-to-p/a-list  ; new for exercise 7.32
 '(what (movie movies) (was were) made between _ and _)
 movies-between-action)

(query-test '((what movies were made between 1970 and 1980))
	    '(((amarcord) (the godfather) (chinatown) (the day of the jackal) 
	       (the exorcist) (gates of heaven) (jaws) (johnny got his gun))))
;
;;; ************************************************************************
;;; Exercise 7.33
;
(display-results "7.33")

(add-to-p/a-list
 '(who is the director of ...)
 director-action)  ; modified for exercise 7.33

(redefine-actors-action-for-7.33)

(add-to-p/a-list
 '(who acted in ...)
 actors-action)    ; modified for exercise 7.33

(query-test '((who is the director of a fish called wanda)
	      (who is the director of fish called wanda)
	      (who acted in the day of the jackal)
	      (who acted in day of the jackal)
	      (who acted in day of jackal))
	    '((michael chrichton)
	      (michael chrichton)
	      ((edward fox) (terence alexander) (michel auclair) (alan badel) 
	       (tony britton) (denis carey) (olga georges-picot) (cyril cusack))
	      ((edward fox) (terence alexander) (michel auclair) (alan badel) 
	       (tony britton) (denis carey) (olga georges-picot) (cyril cusack))
	      ((edward fox) (terence alexander) (michel auclair) (alan badel) 
	       (tony britton) (denis carey) (olga georges-picot) (cyril cusack))))
;
;;; ************************************************************************
;;; Exercise 7.34
;
(display-results "7.34")

(redefine-matches?-for-7.34)
(redefine-substitutions-in-to-match-for-7.34)

(add-to-p/a-list  ; new for exercise 7.34
 '(when was ... made)
 when-made-action)

(query-test '((when was fish called wanda made)
	      (when was day of jackal made))
	    '(1988
	      1973))
;
;
;;; ************************************************************************
;;; Exercise 7.36
;
(display-results "7.36")

(add-to-p/a-list  ; new for exercise 7.36
 '(what (movie movies) (did does) ... appear in between _ and _)
 appear-in-between-action)

(add-to-p/a-list  ; new for exercise 7.36
 '(what (movie movies) (did does) ... direct between _ and _)
 direct-between-action)

(query-test
 '((what movies did forest whitaker appear in between 1980 and 1990)
    (what movies did forest whitaker appear in between 1990 and 2000)
   (what movie does orson welles direct between 1940 and 1950)
   (what movie does orson welles direct between 1950 and 1960)
   (what movies did orson welles direct between 1941 and 1952))
 '((i do not know)
   ((the crying game))
   ((citizen kane))
   ((othello))
   ((citizen kane) (othello))))
;
;
;;; ************************************************************************
;;; Exercise 7.37
;
;(display-results "7.37")
;
(redefine-matches?-for-7.37)
(redefine-substitutions-in-to-match-for-7.37)

(add-to-p/a-list ; modified for ex. 7.37
 '(what (movie movies) (was were) made (in before after since) number?)
 movies-before-after-action)

(add-to-p/a-list ; modified for ex. 7.37
 '(what (movie movies) (was were) made between number? and number?)
 movies-between-action)

(add-to-p/a-list ; modified for ex. 7.37
 '(what (movie movies) (did does) ... appear in between number? and number?)
 appear-in-between-action)

(add-to-p/a-list ; modified for ex. 7.37
 '(what (movie movies) (did does) ... direct between number? and number?)
 direct-between-action)

(add-to-p/a-list ; modified for ex. 7.37
 '(what (movie movies) (was were) made in number?)
 made-in-year-action)

(add-to-p/a-list  ; new for exercise 7.37
 '(what (movie movies) (has have) symbol? in the title)
 word-in-title-action)

(query-test '((what movie was made in 1952)
	      (what movie was made in barcelona)
	      (what movie has gun in the title)
	      (what movies have a in the title))
	    '(((an american in paris) (othello))
	      (i do not know)
	      ((johnny got his gun))
	      ((a star is born) (a fish called wanda))))
;
;
;
;;; ************************************************************************
;;; Exercise 7.35
;
(display-results "7.35")

(redefine-substitutions-in-to-match-for-7.35)

(redefine-answer-by-pattern-for-test)
(redefine-answer-by-pattern)

(add-to-p/a-list    ; new for exercise 7.35
 '(what (movie movies) directed by ... star ...)
 director-and-star-action)

(add-to-p/a-list    ; new for exercise 7.35
 '(what (movie movies) star both ... and ...)
 two-stars-action)

(query-test '((what movies directed by david lean star omar sharif)
	      (what movies star both olivia de havilland and butterfly mcqueen))
	    '(((lawrence of arabia))
	      ((gone with the wind))))


;;; **************************************************************
;;; Uncomment the following if you would like to run the interactive
;;; query loop following completion of the automated tests
;
;;; (query-loop)

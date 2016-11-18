(module movie-test-utilities racket

  (provide query-test 
	   redefine-answer-by-pattern-for-test
	   query-loop
	   redefine-answer-by-pattern)

  (require "movie-adts.scm")
;  (require "lab08.scm")        ; for the matches? and substitutions-in-to-match definitions
 (require "assignment4.scm")  ; for the matches? and substitutions-in-to-match definitions

  ;; *********************************************************************
  ;; For running automated tests in lab08-test.scm or assignment4-test.scm

  (define query-test
    (lambda (queries results)
      (cond ((not (null? queries))
	     (let ((query (car queries))
		   (correct-result (car results)))
	       (let ((your-result (answer-by-pattern-for-test query movie-p/a-list)))
		 (display (format "Query:\n~a\nShould get result:\n~a\nYour result:\n~a\n" 
				  query correct-result your-result))
		 (if (not (equal? your-result correct-result))
		     (error (format "Test failed\n\n"))
		     (display (format "Test OK\n\n")))
		 (query-test (cdr queries) (cdr results))))))))

  ;; Different than the version given in the text.
  ;; Result is returned rather than displayed
  (define answer-by-pattern-for-test
    (lambda (query p/a-list)
      (cond ((null? p/a-list)
	     '(i do not understand))
	    ((matches? (pattern (car p/a-list)) query)
	     (let ((subs (substitutions-in-to-match
			  (pattern (car p/a-list))
			  query)))
	       (let ((result (apply (action (car p/a-list))
				    subs)))
		 (if (null? result)
		     '(i do not know) ;; result is returned rather than displayed
		     result))))       ;; result is returned rather than displayed
	    (else
	     (answer-by-pattern-for-test query
				(cdr p/a-list))))))

  (define redefine-answer-by-pattern-for-test ; eliminates matches?
    (lambda () 
      (set! answer-by-pattern-for-test
	(lambda (query p/a-list)
	  (cond ((null? p/a-list)
		 '(i do not understand))
		(else 
		 (let ((subs (substitutions-in-to-match
			      (pattern (car p/a-list))
			      query)))
		   (if (not (null? subs))
		       (let ((result (apply (action (car p/a-list))
					    subs)))
			 (if (null? result)
			     '(i do not know)
			     result))
		       (answer-by-pattern-for-test query (cdr p/a-list))))))))))


  ;; *********************************************************************
  ;; For running the query loop interactively

  (define query-loop
    (lambda ()
      (newline)
      (newline)
      (let ((query (read)))
	(cond ((exit? query) (display '(see you later)))
	      ;; movie-p/a-list is the list of the
	      ;;  pattern/action pairs
	      (else (answer-by-pattern query movie-p/a-list)
		    (query-loop))))))

  (define exit?
    (lambda (query)
      (member query '((bye)
		      (quit)
		      (exit)
		      (so long)
		      (farewell)))))

  (define answer-by-pattern
    (lambda (query p/a-list)
      (cond ((null? p/a-list)
	     (display '(i do not understand)))
	    ((matches? (pattern (car p/a-list)) query)
	     (let ((subs (substitutions-in-to-match
			  (pattern (car p/a-list))
			  query)))
	       (let ((result (apply (action (car p/a-list))
				    subs)))
		 (if (null? result)
		     (display '(i do not know))
		     (display result)))))
	    (else
	     (answer-by-pattern query
				(cdr p/a-list))))))

  (define redefine-answer-by-pattern ; eliminates matches?
    (lambda () 
      (set! answer-by-pattern
	    (lambda (query p/a-list)
	      (cond ((null? p/a-list)
		     (display '(i do not understand)))
		    (else 
		     (let ((subs (substitutions-in-to-match
				  (pattern (car p/a-list))
				  query)))
		       (if (not (null? subs))
			   (let ((result (apply (action (car p/a-list))
						subs)))
			     (if (null? result)
				 (display '(i do not know))
				 (display result)))
			   (answer-by-pattern query (cdr p/a-list))))))))))

)
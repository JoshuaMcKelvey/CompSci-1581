;; This file contains the arithmetic expression evaluator code.
;; It makes use of the RA-Stack ADT.  It can use any of three
;; implementations of the ADT

(module evaluate racket

  (provide evaluate)

  (require "Ex13.9.scm")

  (define evaluate
    (lambda (expression-string)
      (let ((expr-stack (make-ra-stack)))
	(define process
	  (lambda (rest-of-expr)
	    (let ((next-token (car rest-of-expr)))
	      (cond ((accept? expr-stack next-token)
		     (top-minus expr-stack 0))
		    ((reduce? expr-stack next-token)
		     (reduce! expr-stack)
		     (process rest-of-expr))
		    ((shift? expr-stack next-token)
		     (push! expr-stack next-token)
		     (process (cdr rest-of-expr)))
		    (else  ; error
		     (error "EVALUATE: syntax error"
			    expr-stack rest-of-expr))))))
	(push! expr-stack '$)
	(process (tokenize expression-string)))))

  (define accept?
    (lambda (expr-stack next-token)
      (if (and (number? (top-minus expr-stack 0))
	       (equal? next-token '$))
	  (equal? (top-minus expr-stack 1) '$)
	  #f)))

  (define reduce?
    (lambda (expr-stack next-token)
      (let ((stack-top (top-minus expr-stack 0)))
	(cond ((number? stack-top)
	       (let ((stack-second (top-minus expr-stack 1)))
		 (cond ((equal? next-token '$)
			(operator? stack-second))
		       ((operator? next-token)
			(and (operator? stack-second)
			     (not (lower-precedence?
				   stack-second
				   next-token))))
		       ((equal? next-token 'rparen)
			(operator? stack-second))
		       (else #f))))
	      ((equal? stack-top 'rparen)
	       (or (equal? next-token '$)
		   (operator? next-token)
		   (equal? next-token 'rparen)))
	      (else #f)))))

  (define shift?
    (lambda (expr-stack next-token)
      (let ((stack-top (top-minus expr-stack 0)))
	(cond ((or (operator? stack-top)
		   (member stack-top '($ lparen)))
	       (or (number? next-token)
		   (equal? next-token 'lparen)))
	      ((number? stack-top)
	       (let ((stack-second (top-minus expr-stack 1)))
		 (cond ((operator? next-token)
			(or (not (operator? stack-second))
			    (lower-precedence? stack-second
					       next-token)))
		       ((equal? next-token 'rparen)
			(equal? stack-second 'lparen))
		       (else #f))))
	      (else #f)))))

  (define reduce!
    (lambda (expr-stack)
      (cond ((equal? (top-minus expr-stack 0) 'rparen)
	     (let ((value (top-minus expr-stack 1)))
	       (pop! expr-stack)  ; remove rparen
	       (pop! expr-stack)  ; remove the value
	       (pop! expr-stack)  ; remove lparen
	       (push! expr-stack value)))
	    (else ; a simple arithmetic operation
	     (let ((left-operand  (top-minus expr-stack 2))
		   (operator      (top-minus expr-stack 1))
		   (right-operand (top-minus expr-stack 0)))
	       (pop! expr-stack)  ; remove the right operand
	       (pop! expr-stack)  ; remove the operator
	       (pop! expr-stack)  ; remove the left operand
	       (push! expr-stack
		      ((look-up-value operator)
		       left-operand
		       right-operand)))))))

  (define look-up-value
    (lambda (name)
      (cond ((equal? name '+) +)
	    ((equal? name '*) *)
	    ((equal? name '-) -)
	    ((equal? name '/) /)
	    (else (error "Unrecognized name" name)))))

  (define operator?
    (lambda (token)
      (member token '(+ - * /))))

  (define lower-precedence?
    (lambda (op-1 op-2)
      (and (member op-1 '(+ -))
	   (member op-2 '(* /)))))

  (define operator-char?
    (lambda (char)
      (member char '(#\+ #\- #\* #\/))))

  (define tokenize
    (lambda (input-string)
      (define iter
	(lambda (i prev-state acc-lst)
	  (if (= i (string-length input-string))
	      acc-lst
	      (let ((next-char (string-ref input-string i)))
		(cond ((equal? next-char #\space)
		       (iter (+ i 1) 'read-space
			     acc-lst))
		      ((char-numeric? next-char) ;next-char is a digit
		       (if (equal? prev-state 'read-numeric)
			   ;; continue constructing the number, digit
			   ;; by digit, by adding the current digit
			   ;; to 10 times the amount read so far
			   (iter (+ i 1) 'read-numeric
				 (cons (+ (* 10 (car acc-lst))
					  (digit->number next-char))
				       (cdr acc-lst)))
			   (iter (+ i 1) 'read-numeric
				 (cons (digit->number next-char)
				       acc-lst))))
		      ((operator-char? next-char)
		       (iter (+ i 1) 'read-operator
			     (cons (string->symbol
				    (make-string 1 next-char))
				   acc-lst)))
		      ((equal? next-char #\()
		       (iter (+ i 1) 'read-lparen
			     (cons 'lparen
				   acc-lst)))
		      ((equal? next-char #\))
		       (iter (+ i 1) 'read-rparen
			     (cons 'rparen
				   acc-lst)))
		      (else
		       (error "illegal character in input"
			      next-char)))))))
      (reverse (cons '$ (iter 0 'start '())))))

  (define digit->number
    (lambda (digit-char)
      (string->number (string digit-char))))

)
;;sacred procedures(core)
(define apply-in-underlying-scheme apply)
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
	((variable? exp) (lookup-variable-value exp env))
	((quoted? exp) (text-of-quotation exp))
	((assignment? exp) (eval-assignment exp env))
	((definition? exp) (eval-definition exp env))
	((if? exp) (eval-if exp env))
	((lambda? exp)
	 (make-procedure (lambda-parameters exp)
			 (lambda-body exp)
			 env))
	((begin? exp)
	 (eval-sequence (begin-actions exp) env))
	((cond? exp) (eval (cond->if exp) env))
	((application? exp)
	 (apply (eval (operator exp) env)
		(list-of-values (operands exp) env)))
	(else
	 (error "Unknown expression type -- EVAL" exp))))

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
	 (apply-primitive-procedure procedure arguments))
	((compound-procedure? procedure)
	 (eval-sequence
	  (procedure-body procedure)
	  (extend-environment
	   (procedure-parameters procedure)
	   arguments
	   (procedure-environment procedure))))
	(else
	 (error
	  "Unknown procedure type -- APPLY" procedure))))

;;procedure arguments
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
	    (list-of-values (rest-operands exps) env))))

;;conditionals
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

;;sequences
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
	(else (eval (first-exp exps) env)
	      (eval-sequence (rest-exps exps) env))))

;;assignments and definitions
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
		       (eval (assignment-value exp) env)
		       env)
  'ok)
  
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

;;ex4.1
(define (list-of-values-right-to-left exps env)
  (if (no-operands? exps)
      '()
      (let ((right (list-of-values (rest-operands exps) env))
	    (left (eval (first-operand exps) env)))
	(cons right left)))) ;;it will have '() in the beginning of exps
;;maybe reverse of exps is better variant


;;REPRES EXPS

;;self-evaluating items
(define (self-evaluating? exp)
  (cond ((number? exp) true)
	((string? exp) true)
	(else false)))

(define (variable? exp) (symbol? exp))

;;quotations
(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;;assignments
(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

;;definitions
(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
		   (cddr exp))))

;;lambda expressions
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;;conditionals
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

;;begin
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
	((last-exp? seq) (first-exp seq))
	(else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

;;application
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

;;COND
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
	    (rest (cdr clauses)))
	(if (cond-else-clause? first)
	    (if (null? rest)
		(sequence->exp (cond-actions first))
		(error "ELSE clause isn't last -- COND->IF"
		       clauses))
	    (make-if (cond-predicate first)
		     (sequence->exp (cond-actions first))
		     (expand-clauses rest))))))

;;ex4.2
(define (application? exp) (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

;;ex4.3
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
	((variable? exp) (lookup-variable-value exp env))
	(((let (proc (get 'eval (car exp)))
	    proc))
	 (proc exp env))
	((application? exp)
	 (apply (eval (operator exp) env)
		(list-of-values (operands exp) env)))
	(else
	 (error "Unknown expression type -- EVAL" exp))))

;;wrappers for special functions
(define (eval-quote exp env)
  (text-of-quotation exp))
(put 'eval 'quote eval-quote)

(define (eval-lambda exp env)
  (make-procedure (lambda-parameters exp)
		  (lambda-body exp)
		  env))
(put 'eval 'lambda eval-lambda )

(define (eval-begin exp env)
  (eval-sequence (begin-actions exp) env))
(put 'eval 'begin (eval-begin exp env))

(define (eval-cond exp env)
  (eval (cond->if exp) env))
(put 'eval 'cond eval-cond)

;;and other puts with original functions implemented above...

;;ex4.4
(define (eval-and exp env)
  (lambda (args)
    (cond (null? args)
	  true
	  (if (true? (eval (car args) env))
	      (eval-and (cdr exp) env)
	      false)))
  (cdr exp))
(put 'eval 'and eval-and)

(define (eval-or exp env)
  (lambda (args)
    (cond (null? args)
	  false
	  (if (true? ((eval (car args) env))
		     true
		     (eval-or (cdr exp) env)))))
  (cdr exp))
(put 'eval 'or eval-or)

;;as derived expressions
(define (and-clauses exp)
  (cdr exp))
(define (and->if exp)
  (expand-and-clauses (and-clauses exp)))

(define (expand-and-clauses clauses)
  (if (null? clauses)
      true
      (let ((first (car clauses))
	    (rest (cdr clauses)))
	(make-if first
		 (expand-and-clauses (cdr clauses))
		 false))))

(define (or-clauses exp)
  (cdr exp))
(define (or->if exp)
  (expand-or-clauses (or-clauses exp)))

(define (expand-or-clauses clauses)
  (if (null? clauses)
      false
      (let ((first (car clauses))
	    (rest (cdr clauses)))
	(make-if first
		 true
		 (expand-or-clauses (cdr clauses))))))

;;ex4.5
(cond ((assoc 'b '((a 1) (b 2))) => cadr)
      (else false))

(define (cond-=>-clause? clause)
  (eq? (cadr clause) '=>))
(define (cond-=>-value clause)
  (car clause))
(define (cond-=>-proc clause)
  (caddr clause))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
	    (rest (cdr clauses)))
	(if (cond-else-clause? first)
	    (if (null? rest)
		(sequence->exp (cond-actions first))
		(error "ELSE clause isn't last -- COND->IF"
		       clauses))
	    (if (cond-=>-clause? first)
		(make-if (cond-predicate first)
			 (list (cond-=>-proc first) (cond-=>-value first))
			 (expand-clauses rest))
		(make-if (cond-predicate first)
			 (sequence->exp (cond-actions first))
			 (expand-clauses rest)))))))

;;ex4.6
;;to add into eval(or into the table if we use data-directed style)
((let? exp) (eval (let->combination exp) env))

;;representation of let
(define (let? exp)
  (tagged-list? exp 'let))
(define (let->combination exp)
  (cons (make-lambda (let-parameters exp)
		     (let-body exp))
	(let-values exp)))

(define (let-parameters exp)
  (map car (cadr exp)))
(define (let-values exp)
  (map cadr (cadr exp)))
(define let-body cddr)

(let-body '(let ((a 1) (b 2) (c 3)) (+ a b c)))

;;ex4.7
;;to eval
((let*? exp) (eval (let*->nested-lists exp) env))
(define (let*? exp)
  (tagged-list? exp 'let*))
(define (let*-inits exp)
  (cadr exp))
(define (let*-body exp)
  (caddr exp))
(define (let*->nested-lets exp)
  (let ((inits (let*-inits exp))
	(body (let*-body exp)))
    (define (make-nested-lets inits)
      (if (null? inits)
	  body
	  (list 'let (list (car inits))
		(make-nested-lets (cdr inits)))))
    (make-nested-lets inits)))

;;ex4.8
(define (fib n)
  (define (fib-iter a b count)
    (if (= 0 count)
	b
	(fib-iter (+ a b) a (-1+ count))))
  (fib-iter 1 0 n))

(define (fib n)
  (let fib-iter ((a 1) (b 0) (count n))
    (if (= 0 count)
	b
	(fib-iter (+ a b) a (-1+ count)))))

;;modification of let->combination
(define (let-named? exp)
  (string? cadr exp))
(define (let-name exp)
  (cadr exp))
(define (let-named-parameters exp)
  (map car (caddr exp)))
(define (let-named-values exp)
  (map cadr (caddr exp)))
(define let-named-body cdddr)
  
(define (let->combination exp)
  (if (let-named? exp)
      (sequence->exp
       (list (list 'define (cons (let-name exp)
				 (let-named-parameters exp))
		   (let-named-body exp))
	     (cons let-name (let-named-values))))
      (cons (make-lambda (let-parameters exp)
			 (let-body exp))
	    (let-values exp))))

;;EVALUATOR DATA STRUCTURES

;;testing on predicates
(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))

;;repres procs
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

;;operations on environments
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
	  (error "Too many arguments supplied" vars vals)
	  (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
	     (env-loop (enclosing-environment env)))
	    ((eq? var (car vars))
	     (car vals))
	    (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
	(error "Unbounda variable" var)
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame)
		(frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
	     (env-loop (enclosing-environment env)))
	    ((eq? var (car vars))
	     (set-car! vals val))
	    (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable -- SET!" var)
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame)
		(frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
	     (add-binding-to-frame! var val frame))
	    ((eq? var (car vars))
 	     (set-car! vals val))
	    (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
	  (frame-values frame))))

;;ex4.11
(define (make-frame variables values)
  (map cons variables values))
;;frame variables, frame values are not necessary with above make-frame
(define (add-binding-to-frame! var val frame)
  (set! frame (cons (cons var val) frame)))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((null? frame)
	     (env-loop (enclosing-environment env)))
	    ((eq? var (caar frame))
	     (cdar frame))
	    (else (scan (cdr frame)))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable" var)
	(let ((frame (first-frame env)))
	  (scan frame))))
  (env-loop env))

;;should rewrite scan similar to above lookup-variable-value
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((null? vars)
	     (env-loop (enclosing-environment env)))
	    ((eq? var (car vars))
	     (set-car! vals val))
	    (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable -- SET!" var)
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame)
		(frame-values frame)))))
  (env-loop env))

;;the same for this function
(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
	     (add-binding-to-frame! var val frame))
	    ((eq? var (car vars))
	     (set-car! vals val))
	    (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
	  (frame-values frame))))

;;ex4.13
(define (make-unbound! var env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
	     (error "Unbound variable"))
	    ((eq? var (car vars))
	      (set! vars (cdr vars))
	      (set! vals (cdr vals)))
	    (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
	  (frame-values frame))))

;;running the evaluator
(define (setup-environment)
  (let ((initial-env
	 (extend-environment (primitive-procedure-names)
			     (primitive-procedure-objects)
			     the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment (setup-environment))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
	(list 'cdr cdr)
	(list 'cons cons) 
	(list 'null? null?)
	(list '< <)))

(define (primitive-procedure-names)
  (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure)
	       (procedure-parameters object)
	       (procedure-body object)
	       '<procedure-env>)
      (display object)))


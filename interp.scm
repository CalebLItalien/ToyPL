;; LET-interp-starter.scm

(load "helpers.scm")

;; ================ Parser Definitions ==================================

;; This defines the translation from the concrete syntax to the abstract syntax.

(define the-grammar
  '(
    (program                        ;; <Program> ::= 
     (expression)                   ;;   Concrete    <Expression>
     a-prog)                        ;;   Abstract    (a-prog exp)
    
    (expression                     ;; <Expression> ::= 
     (number)                       ;;   Concrete       <Number> 
     const-exp)                     ;;   Abstract       (const-exp num)
    
    (expression                     ;; <Expression> ::= 
     ("zero?(" expression ")")      ;;   Concrete       zero?(<Expression>)
     zero?-exp)                     ;;   Abstract       (zero?-exp exp)
    
    (expression                                             ;; <Expression> ::= 
     ("if" expression "then" expression "else" expression)  ;;   Concrete       if <Expression> then <Expression> else <Expression>
     if-exp)                                                ;;   Abstract       (if-exp exp1 exp2 exp3)
        
    (expression                     ;; <Expression> ::= 
     (identifier)                   ;;   Concrete       <Identifier>
     var-exp)                       ;;   Abstract       (var-exp var)
    
    (expression                                          ;; <Expression> ::= 
     ("let" identifier "=" expression "in" expression)   ;;   Concrete       let <Identifier> = <Expression> in <Expression>
     let-exp)                                            ;;   Abstract       (let-exp var exp1 exp2)

    ;; ============== PROC Definitions below (updated by HW 6) ========================

    (expression                                     ;; <Expression> ::=
     ("proc" "(" (arbno identifier) ")" expression) ;;   Concrete       proc (<Identifier>*) <Expression>
     proc-exp)                                      ;;   Abstract       (proc-exp vars exp) 
    
    (expression                                    ;; <Expression> ::= 
     ("(" expression (arbno expression) ")")       ;;   Concrete       (<Expression> <Expression>*)
     call-exp)                                     ;;   Abstract       (call-exp exp exps)

    ;; ============== LETREC Definitions below (updated by HW 6) ========================
    
    (expression                                    ;; <Expression> ::=                                 
     ("letrec" identifier "(" (arbno identifier)   ;;   Concrete       letrec <Identifier>(<Identifier>*) = <Expression> in <Expression>
      ")" "=" expression "in" expression)          ;;   Abstract       (letrec-exp p-name p-vars p-body body)
     letrec-exp)

    ;; ============== HW 5 Definitions below ========================

    (program                               ;; <Program> ::= 
     ("def!" identifier "=" expression)    ;;  Concrete     def! <Identifier> = <Expression>
     def-prog)                             ;;  Abstract     (def-prog var exp)
    
    (expression                            ;; <Expression> ::= 
     ("true")                              ;;   Concrete       #true
     const-true-exp)                       ;;   Abstract       (const-true-exp)
    
    (expression                            ;; <Expression> ::=
     ("false")                             ;;   Concrete       #false
     const-false-exp)                      ;;   Abstract       (const-false-exp)
     
    (expression                            ;; <Expression> ::= 
     ("*(" expression "," expression ")")  ;;   Concrete       *(<Expression>,<Expression>)
     times-exp)                            ;;   Abstract       (times-exp exp1 exp2)
    
    (expression                            ;; <Expression> ::= 
     ("/(" expression "," expression ")")  ;;   Concrete       /(<Expression>,<Expression>)
     div-exp)                              ;;   Abstract       (div-exp exp1 exp2)
    
    (expression                            ;; <Expression> ::= 
     ("-(" expression "," expression ")")  ;;   Concrete       -(<Expression>,<Expression>)
     diff-exp)                             ;;   Abstract       (diff-exp exp1 exp2)
    
    (expression                            ;; <Expression> ::= 
     ("+(" expression "," expression ")")  ;;   Concrete       +(<Expression>,<Expression>)
     plus-exp)                             ;;   Abstract       (plus-exp exp1 exp2)
    
    (expression                            ;; <Expression> ::= 
     ("=(" expression "," expression ")")  ;;   Concrete       =(<Expression>,<Expression>)
     equal-exp)                            ;;   Abstract       (equal-exp exp1 exp2)

    (expression                            ;; <Expression> ::= 
     ("<(" expression "," expression ")")  ;;   Concrete       <(<Expression>,<Expression>)
     less-than-exp)                        ;;   Abstract       (less-than-exp exp1 exp2)
    
    (expression                            ;; <Expression> ::= 
     ("and(" expression "," expression ")") ;;   Concrete      and(<Expression>,<Expression>)
     and-exp)                              ;;   Abstract       (and-exp exp1 exp2)

    (expression                            ;; <Expression> ::= 
     ("or(" expression "," expression ")") ;;   Concrete       or(<Expression>,<Expression>)
     or-exp)                               ;;   Abstract       (or-exp exp1 exp2)
    
    (expression                            ;; <Expression> ::= 
     ("not(" expression ")")               ;;   Concrete       not(<Expression>)
     not-exp)                              ;;   Abstract       (not-exp exp)

    (expression                               ;; <Expression> ::=
     ("cons(" expression "," expression ")")  ;;   Concrete    cons(<Expression>,<Expression>)
     cons-exp)                                ;;   Abstract    (cons-exp exp1 exp2)
    
    (expression                            ;; <Expression> ::=
     ("car(" expression ")")               ;;   Concrete       car(<Expression>)
     car-exp)                              ;;   Abstract       (car-exp exp)
 
    (expression                            ;; <Expression> ::=
     ("cdr(" expression ")")               ;;   Concrete       cdr(<Expression>)
     cdr-exp)                              ;;   Abstract       (cdr-exp exp)
 
    (expression                            ;; <Expression> ::=
     ("null?(" expression ")")             ;;   Concrete       null?(<Expression>)
     null?-exp)                            ;;   Abstract       (null?-exp exp)
 
    (expression                            ;; <Expression> ::=
     ("emptylist")                         ;;   Concrete       emptylist
     emptylist-exp)                        ;;   Abstract       (emptylist-exp)

    ;; ============== REF Definitions below ========================
    
    (expression                            ;; <Expression> ::=
     ("newref!(" expression ")")           ;;   Concrete       newref!(<Expression>)
     newref!-exp)                          ;;   Abstract       (newref!-exp exp)
    
    (expression                                 ;; <Exp>   ::= 
     ("setref!(" expression "," expression ")") ;; Concrete    setref!(<Expression>,<Expression>)
     setref!-exp)                               ;; Abstract    (setref!-exp exp1 exp2)
    
    (expression                            ;; <Expression> ::=
     ("deref(" expression ")")             ;;   Concrete       deref(<Expression>)
     deref-exp)                            ;;   Abstract       (deref-exp exp)
    
    ;; ============== IMP Definitions below ========================

    (expression                            ;; <Expression> ::=
     ("set!" identifier "=" expression)    ;;   Concrete       set! <Identifier> = <Expression>
     set!-exp)                             ;;   Abstract       (set!-exp var exp)
    
    ;; ============== HW 6 Definitions below ========================

    (expression                                           ;; <Expression> ::=
     ("{" (arbno expression) "}")                         ;;   Concrete       {<Expression>*}
     block-exp)                                           ;;   Abstract       (block-exp exps)
    
    (expression                                           ;; <Expression> ::=
     ("print!" "(" expression ")")                        ;;   Concrete       print!(<Expression>)
     print-exp)                                           ;;   Abstract       (print-exp exp)                         

    (expression                                           ;; <Expression> ::=
     ("newline!")                                         ;;   Concrete       newline!
     newline-exp)                                         ;;   Abstract       (newline-exp) 
    ))

;; Sets up the parser using the above concrete <-> abstract grammars.
;; Defines a function call parse that takes a string in the concrete
;; syntax and returns the corresponding abstract syntax tree. 
(load "lex-scan-parse.scm")

;; ==================== Expressed Values ==================================

(define-datatype expval expval?
  [num-val
    [num number?]]
  [bool-val
    [b boolean?]]
  [proc-val
    [var (list-of symbol?)]
    [body expression?]
    [saved-env environ?]]
  [ref-val
    [ref integer?]]
  [unit-val]
  )

(define expval->num
  (lambda (ev)
    (cases expval ev
	   [num-val (num) num]
      [else (raise-exception 'expval->num "Expressed value is not a number: ~s" ev)])))

(define expval->bool
  (lambda (ev)
    (cases expval ev
	   [bool-val (b) b]
      [else (raise-exception 'expval->bool "Expressed value is not a Boolean: ~s" ev)])))

(define expval->ref
  (lambda (ev)
    (cases expval ev
	   [ref-val (ref) ref]
	   [else (raise-exception 'expval->ref "Expressed value is not a reference: ~s" ev)])))

(define expval->proc-body
  (lambda (ev)
    (cases expval ev
	   [proc-val (param body env) body]
	   [else (raise-exception 'expval->num "Expressed value is not a procedure: ~s" ev)])))

(define expval->proc-param
  (lambda (ev)
    (cases expval ev
	   [proc-val (param body env) param]
	   [else (raise-exception 'expval->num "Expressed value is not a procedure: ~s" ev)])))

(define expval->proc-saved-env
  (lambda (ev)
    (cases expval ev
	   [proc-val (param body env) env]
	   [else (raise-exception 'expval->num "Expressed value is not a procedure: ~s" ev)])))

(define expval->string
  (lambda (ev)
    (cases expval ev
      [num-val (num) (number->string num)]
      [bool-val (b) (if b "#true" "#false")]
      [proc-val (var body saved-env) "#proc"]
      [ref-val (ref) (string-append "#ref(" (number->string ref) ")")]
      [unit-val () ""]
      [else (raise-exception 'expval->string
              "Expressed value is not a number or Boolean: ~s" ev)])))

;; =================== Environment ========================================

(define ref-val?
  (lambda (ev)
    (cases expval ev
      [ref-val (ref) #t]
      [else #f])))

(define-datatype environ environ?
  [empty-env]
  [extend-env
   [var symbol?]
   [val ref-val?] ;; Expressed values and denoted values are the same (for now).
   [env environ?]]
  [extend-env-rec
    [f-name symbol?]
    [f-var (list-of symbol?)]
    [f-body expression?]
    [env environ?]]
  )

(define apply-env ; Env x Var -> Expval
  (lambda (env target-var)
    (cases environ env
      [empty-env () (raise-exception 'apply-env "No binding for ~s" target-var)]
      [extend-env (var val env)
        (if (equal? var target-var)
          val
          (apply-env env target-var))]
      [extend-env-rec (f-name f-var f-body env)
        (if (equal? f-name target-var)
          ;; #t case
          (newref! (proc-val f-var f-body (extend-env-rec f-name f-var f-body env)))
          ;; #f case
          (apply-env env target-var))]
      )))

(define make-init-env
  (lambda ()
    (extend-env 
     'pi (newref! (num-val 3.14159))
     (extend-env
      'e (newref! (num-val 2.71828))
      (empty-env)))))

(define env->string
  (lambda (env)
    (cases environ env
      [empty-env () "[]"]
      [extend-env (var val env*)
                  (string-append "[" (symbol->string var) 
		                 " = "  (expval->string (deref val)) 
		                 (env->string* env*) "]")]
      [extend-env-rec (f-name f-vars f-body env*)
                      (string-append "[" f-name "("
                                     (fold-left string-append "" (map symbol->string f-vars)) ")"
                                     (env->string* env*)"]")])))

(define env->string*
  (lambda (env)
    (cases environ env
      [empty-env () ""]
      [extend-env (var val env*) 
	(string-append ", " (symbol->string var) 
		       " = " (expval->string (deref val)) 
		       (env->string* env*))]
      [extend-env-rec (f-name f-vars f-body env*)
                      (string-append "," f-name "("
                                     (fold-left string-append "" (map symbol->string f-vars)) ")"
                                     (env->string* env*))])))
                      
;; ==================== Store =========================================

(load "store.scm")

;; ==================== Evaluater =========================================

(define value-of-prog
  (lambda (prog env)
    (cases program prog
           [a-prog (exp) (cons (value-of-exp exp env) env)]
           [def-prog (var exp) (cons (unit-val) (extend-env var (newref! (value-of-exp exp env)) env))]
           [else (raise-exception
                  'value-of-prog "Abstract syntax case not implemented: ~s" (car prog))])))

(define value-of-exp
  (lambda (exp env)
    (cases expression exp
      ;; In class
      [const-exp (num) (num-val num)]
      [diff-exp (exp1 exp2) (num-val (- (expval->num (value-of-exp exp1 env)) (expval->num (value-of-exp exp2 env))))]
      [zero?-exp (exp1) (bool-val (= (expval->num (value-of-exp exp1 env)) 0))]
      [if-exp (exp1 exp2 exp3) (if (expval->bool (value-of-exp exp1 env)) (value-of-exp exp2 env) (value-of-exp exp3 env))]
      [var-exp (var) (deref (apply-env env var))]
      [let-exp (var exp1 exp2) (value-of-exp exp2 (extend-env var (newref! (value-of-exp exp1 env)) env))]
      
      ;; ============ HW 5 Interpreter ==============
      [const-true-exp () (bool-val #t)]
      [const-false-exp () (bool-val #f)]
      [plus-exp (exp1 exp2) (num-val (+ (expval->num (value-of-exp exp1 env)) (expval->num (value-of-exp exp2 env))))]
      [div-exp (exp1 exp2)
	       (let 
		   ([val1 (expval->num (value-of-exp exp1 env))]
		    [val2 (expval->num (value-of-exp exp2 env))])
		 (if (= val2 0)
		     (raise-exception 'value-of-exp "Divide by zero exp = ~s with env = ~s" exp env)
		     (num-val (/ val1 val2))))]
      [times-exp (exp1 exp2) (num-val (* (expval->num (value-of-exp exp1 env)) (expval->num (value-of-exp exp2 env))))]
      [less-than-exp (exp1 exp2) (bool-val (< (expval->num (value-of-exp exp1 env)) (expval->num (value-of-exp exp2 env))))]
      [equal-exp (exp1 exp2) (bool-val (= (expval->num (value-of-exp exp1 env)) (expval->num (value-of-exp exp2 env))))]      
      [and-exp (exp1 exp2) (bool-val (and (expval->bool (value-of-exp exp1 env)) (expval->bool (value-of-exp exp2 env))))]
      [or-exp (exp1 exp2) (bool-val (or (expval->bool (value-of-exp exp1 env)) (expval->bool (value-of-exp exp2 env))))]
      [cons-exp (exp1 exp2) (list-val (cons (value-of-exp exp1 env) (expval->list (value-of-exp exp2 env)))) ]
      [not-exp (exp1) (bool-val (not (expval->bool (value-of-exp exp1 env))))]
      [car-exp (exp1)
	       (let
		   ([ls (expval->list (value-of-exp exp1 env))])
		 (if (null? ls)
		     (raise-exception 'value-of-exp "Attempting to car empty list." exp env))
		 (car ls))]
      [cdr-exp (exp1) (list-val (cdr (expval->list (value-of-exp exp1 env))))]
      [null?-exp (exp1) (bool-val (null? (expval->list (value-of-exp exp1 env))))]
      [emptylist-exp () (list-val '())]

      ;; ============ HW 6 Interpreter ==============
      [proc-exp (vars body) (proc-val vars body env)]                                      
      [call-exp (exp exps)
		(let*
		    ([val (value-of-exp exp env)]
		     [params (expval->proc-param val)]
		     [body (expval->proc-body val)]
		     [saved-env (expval->proc-saved-env val)])
		  (cond
		   [(= (length params) (length exps))
		    (let
			([vals (map (lambda (x) (value-of-exp x env)) exps)])
		      (value-of-exp body 
				    (fold-left
				     (lambda (acc head) (extend-env (car head) (newref! (cdr head)) acc)) 
				     saved-env
				     (reverse (map (lambda (param val) (cons param val)) params vals)))))]
		   [else (raise-exception 'value-of-exp 
					  "Attempt to apply function with inconsistent number of arguments: ~s ~s." exp exps)]))]
      [letrec-exp (f-name f-vars f-body body)                                                         
		  (value-of-exp body (extend-env-rec f-name f-vars f-body env))]     

      [print-exp (exp) (display (expval->string (value-of-exp exp env))) (unit-val)]
      [newline-exp ()  (newline) (unit-val)]

      [block-exp (exps) (fold-left (lambda (acc exp) (value-of-exp exp env)) (unit-val) exps)]

      ;; ============ REF Interpreter ==============
      [newref!-exp (exp) (newref! (value-of-exp exp env))]

      [deref-exp (exp) (deref (value-of-exp exp env))]

      [setref!-exp (ref exp)
        (setref! (value-of-exp ref env) (value-of-exp exp env))
        (unit-val)]

      ;; ============ IMP Interpreter ==============
      [set!-exp (var exp)
        (setref!
          (apply-env env var) ;; Get the ref for the variable from the env.
          (value-of-exp exp env) ;; Set to this value.
          )]
      
      [else
        (raise-exception
          'value-of-exp
          "Abstract syntax case not implemented: ~s" (car exp))])))


;; =================== Interpreter =========================================

;; (start) -- Starts the interpreter.
(define start
  (lambda ()
    (begin
      (initialize-store!)
      (display "\n=== Welcome to the LET + LETREC + PROC + HW5 + HW6 + IMP + REF Interpreter === \n\n")
      (read-eval-print (make-init-env)))))

;; (read-eval-print) -- Main read, eval, and print loop.
(define read-eval-print
  (lambda (env)
    ;; Read a line user input
    (let ([concrete-code (get-input-string)])
      (cond
       [(equal? concrete-code "!quit")  
        (display "Goodbye!")  ;; Quit if 'quit entered.
        (newline)]
       [else
	(cond
    [(equal? concrete-code "!display-store")
      (display-store)  
      (newline)]
	 [(equal? concrete-code "!debug0")
	  (untrace value-of value-of-exp)
	  (untrace expval->num expval->bool expval->string)]
	 [(equal? concrete-code "!debug1")
	  (trace value-of value-of-exp)
	  (untrace expval->num expval->bool expval->string)]
	 [(equal? concrete-code "!debug2")
	  (trace value-of value-of-exp expval->num expval->bool expval->string)]
	 [(equal? concrete-code "!debug3")
	  (trace value-of value-of-exp expval->num expval->bool expval->string apply-env extend-env empty-env)]
	 [(equal? concrete-code "!env")
	  (display (env->string env))
	  (newline)]
	 [(equal? concrete-code "!reset-env")
	  (set! env (make-init-env))]
	 [else
          (guard
           [parse-ex [else (display-exception parse-ex)]]
           ;; Parse code, eval expression, and print result.
           (let
               ([abstract-code (parse concrete-code)])
             (guard
              [value-of-ex [else (display-exception value-of-ex)]]
              (let*
		  ([result (value-of-prog abstract-code env)]
		   [val (car result)]
		    [new-env (cdr result)])
		(display (expval->string val))
		(set! env new-env)  
		(newline)
		))))])
        ;; "Loop".  Notice it is tail recursive.
        (read-eval-print env)]))))
(module top (lib "eopl.ss" "eopl")
  
  ;; top level module.  Loads all required pieces.
  ;; Run the test suite with (run-all).
  
  (require "drscheme-init.scm")
  (require "data-structures.scm")  ; for expval constructors
  (require "environments.scm")
  
  ;; since this is the top-level module, we don't really need to
  ;; provide anything, but we do so just in case.  
  
  (provide run run-all)
  
  (provide test-all)
  
  (define (test-all) (run-all))
  
  ;; here are some other things that could be provided:
  
  ;;   (provide (all-defined-out))
  ;;   (provide (all-from "interp.scm"))
  ;;   (provide (all-from "lang.scm"))
  
  ;;;;;;;;;;;;;;;; interface to test harness ;;;;;;;;;;;;;;;;
  
  ;; run : String -> ExpVal
  ;; Page: 71
  (define run
    (lambda (string)
      (value-of-program (scan&parse string))))
  
  ;; run-all : () -> unspecified
  
  ;; runs all the tests in test-list, comparing the results with
  ;; equal-answer?  
  
  (define run-all
    (lambda ()
      (run-tests! run equal-answer? test-list)))
  
  (define equal-answer?
    (lambda (ans correct-ans)
      (equal? ans (sloppy->expval correct-ans))))
  
  (define sloppy->expval 
    (lambda (sloppy-val)
      (cond
        ((number? sloppy-val) (num-val sloppy-val))
        ((boolean? sloppy-val) (bool-val sloppy-val))
        ((list? sloppy-val) (list->list-val sloppy-val))
        (else
         (eopl:error 'sloppy->expval 
                     "Can't convert sloppy value to expval: ~s"
                     sloppy-val)))))
  
  ;; run-one : symbol -> expval
  
  ;; (run-one sym) runs the test whose name is sym
  
  (define run-one
    (lambda (test-name)
      (let ((the-test (assoc test-name test-list)))
        (cond
          ((assoc test-name test-list)
           => (lambda (test)
                (run (cadr test))))
          (else (eopl:error 'run-one "no such test: ~s" test-name))))))
  
  
  (define the-lexical-spec
    '((whitespace (whitespace) skip)
      (comment ("%" (arbno (not #\newline))) skip)
      (identifier
       (letter (arbno (or letter digit "_" "-" "?")))
       symbol)
      (number (digit (arbno digit)) number)
      (number ("-" digit (arbno digit)) number)
      ))
  
  (define the-grammar
    '((program (expression) a-program)
      
      (expression 
       (number) 
       const-exp)
      
      (expression 
       (identifier) 
       var-exp)
      
      (expression
       ("-" "(" expression "," expression ")")
       diff-exp)
      
      (expression 
       ("+" "(" expression "," expression ")") 
       add-exp)
      
      (expression
       ("*" "(" expression "," expression ")") 
       mulit-exp)
      
      (expression
       ("quotient" "(" expression "," expression ")") 
       quotient-exp)
      
      (expression 
       ("minus" "(" expression ")") 
       minus-exp)
      
      (expression
       ("if" bool-expression "then" expression "else" expression)
       if-exp)
      
      (expression
       ("let" (arbno identifier "=" expression) "in" expression)
       let-exp)   
      
      (expression
       ("let*" (arbno identifier "=" expression) "in" expression)
       let*-exp)
      
      (expression 
       ("cons" "(" expression "," expression ")")
       cons-exp)
      
      (expression
       ("car" "(" expression ")")
       car-exp)
      
      (expression
       ("cdr" "(" expression ")")
       cdr-exp)
      
      (expression
       ("emptylist")
       emptylist-exp)
      
      (expression
       ("list" "(" (separated-list expression ",") ")")
       list-exp)
      
      (expression
       ("cond"  (arbno bool-expression "==>" expression) "end")
       cond-exp)
      
      (expression 
       ("print" "(" expression ")")
       print-exp)
      
      (expression
       ("unpack" (arbno identifier) "=" expression "in" expression)
       unpack-exp)
      
      (expression 
       (bool-expression) 
       a-bool-expression)
      
      ;; bool-expression
      (bool-expression 
       ("equal?" "(" expression "," expression ")")
       equal?-exp)
      
      (bool-expression 
       ("greater?" "(" expression "," expression ")")
       greater?-exp)
      
      (bool-expression
       ("less?" "(" expression "," expression ")")
       less?-exp)
      
      (bool-expression
       ("zero?" "(" expression ")") 
       zero?-exp)
      ))
  
  ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;
  
  (sllgen:make-define-datatypes the-lexical-spec the-grammar)
  
  (define show-the-datatypes
    (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))
  
  (define scan&parse
    (sllgen:make-string-parser the-lexical-spec the-grammar))
  
  (define just-scan
    (sllgen:make-string-scanner the-lexical-spec the-grammar))
  
  
  (define value-of-program
    (lambda (exp)
      (cases program exp
        (a-program (program-exp) (value-of program-exp (init-env))))))
  
  (define value-of
    (lambda (exp env)
      (cases expression exp
        (const-exp (exp1) (num-val exp1))
        
        (a-bool-expression (exp1) (value-of-bool-expression exp1 env))
        
        (diff-exp (exp1 exp2) 
                  (num-val (- 
                            (expval->num (value-of exp1 env)) 
                            (expval->num (value-of exp2 env)))))
        
        (add-exp (exp1 exp2) 
                 (num-val (+ 
                           (expval->num (value-of exp1 env)) 
                           (expval->num (value-of exp2 env)))))
        
        (mulit-exp (exp1 exp2) 
                   (num-val (* 
                             (expval->num (value-of exp1 env)) 
                             (expval->num (value-of exp2 env)))))
        
        (quotient-exp (exp1 exp2) 
                      (num-val (quotient 
                                (expval->num (value-of exp1 env)) 
                                (expval->num (value-of exp2 env)))))
        
        
        (var-exp (exp1) (apply-env env exp1))
        
        (if-exp (exp1 exp2 exp3) (if (expval->bool (value-of-bool-expression exp1 env)) 
                                     (value-of exp2 env)
                                     (value-of exp3 env)))
        
        (minus-exp (exp1) (num-val (- 0 (expval->num (value-of exp1 env)))))
        
        (let-exp (vars exps body) (value-of-let vars exps body env))
        
        (let*-exp (vars exps body) (value-of-let* vars exps body env))
        
        (emptylist-exp () empty-list-val)
        
        (car-exp (exp1) (list-val (car (expval->list (value-of exp1 env)))))
        
        (cdr-exp (exp1) (list-val (cdr (expval->list (value-of exp1 env)))))
        
        (list-exp (exps) (list-val (value-of-exps exps env)))
        
        (cons-exp (exp1 exp2) (list-val (cons (value-of exp1 env)
                                              (expval->list (value-of exp2 env)))))
        
        (cond-exp (exps1 exps2) (value-of-cond exps1 exps2 env))
        
        (print-exp (exp1) (value-of-print (value-of exp1 env)))
        
        (unpack-exp (vars list-val-exp exp) (value-of-unpack vars list-val-exp exp env))
        
        )))
  
  (define value-of-bool-expression
    (lambda (exp env)
      (cases bool-expression exp
        (equal?-exp (exp1 exp2) 
                    (bool-val (equal? 
                               (expval->num (value-of exp1 env)) 
                               (expval->num (value-of exp2 env)))))
        
        (greater?-exp (exp1 exp2) 
                      (bool-val (>
                                 (expval->num (value-of exp1 env)) 
                                 (expval->num (value-of exp2 env)))))
        
        (less?-exp (exp1 exp2) 
                   (bool-val (< 
                              (expval->num (value-of exp1 env)) 
                              (expval->num (value-of exp2 env)))))
        
        (zero?-exp (exp1) (bool-val (= (expval->num (value-of exp1 env)) 0)))
        
        )))
  
  (define value-of-unpack
    (lambda (vars list-val-exp exp env)
      (if (repeat-in-list vars)
          (eopl:error 'unpack "identifer repeat")
          (let ((list-val (expval->list (value-of list-val-exp env))))
            (if (not(= (length vars) (length list-val)))
                (eopl:error 'unpack "identifer number no equal to list")
                (value-of exp (unpack-extend-env vars list-val env)))))))
  
  (define unpack-extend-env
    (lambda (vars list-val env)
      (if (null? vars)
          env
          (unpack-extend-env (cdr vars) 
                             (cdr list-val) 
                             (extend-env (car vars) (car list-val) env)))))
  
  (define value-of-print
    (lambda (expval)
      (eopl:printf "print ~s~%" (expval->val expval))
      (num-val 1)))
  
  (define value-of-cond
    (lambda (conds results env)
      (if (null? conds)
          (eopl:error 'value-of-cond "no one branch is true")
          (if (expval->bool (value-of-bool-expression (car conds) env))
              (value-of (car results) env)
              (value-of-cond (cdr conds) (cdr results) env)))))
  
  (define value-of-exps
    (lambda (exps env)
      (if (null? exps)
          '()
          (cons (value-of (car exps) env)
                (value-of-exps (cdr exps) env)))))
  
  (define value-of-let
    (lambda (vars exps body env)
      (if (repeat-in-list vars)
          (eopl:error 'let "identifer repeat")
          (value-of body (let-extend-env vars exps env)))))
  
  (define let-extend-env
    (lambda (vars exps env)
      (if (null? vars)
          env
          (extend-env (car vars) 
                      (value-of (car exps) env) 
                      (let-extend-env (cdr vars) (cdr exps) env)))))
  
  (define value-of-let*
    (lambda (vars exps body env)
      (if (repeat-in-list vars)
          (eopl:error 'let* "identifer repeat")
          (value-of body (let*-extend-env vars exps env)))))
  
  (define let*-extend-env
    (lambda (vars exps env)
      (if (null? vars)
          env              
          (let*-extend-env (cdr vars) 
                           (cdr exps) 
                           (extend-env (car vars) (value-of (car exps) env) env)))))
  
  (define repeat-in-list
    (lambda (list)
      (if (null? list)
          #f
          (if (find-in-list (car list) (cdr list))
              #t
              (repeat-in-list (cdr list))))))
  
  (define find-in-list
    (lambda (var list)
      (if (null? list)
          #f
          (if (equal? var (car list))
              #t
              (find-in-list var (cdr list))))))
  
  
  (define test-list
    '(
      
      ;; simple arithmetic
      (positive-const "11" 11)
      (negative-const "-33" -33)
      (simple-arith-1 "-(44,33)" 11)
      
      ;; nested arithmetic
      (nested-arith-left "-(-(44,33),22)" -11)
      (nested-arith-right "-(55, -(22,11))" 44)
      
      ;; simple variables
      (test-var-1 "x" 10)
      (test-var-2 "-(x,1)" 9)
      (test-var-3 "-(1,x)" -9)
      
      ;; simple unbound variables
      (test-unbound-var-1 "foo" error)
      (test-unbound-var-2 "-(x,foo)" error)
      
      ;; simple conditionals
      (if-true "if zero?(0) then 3 else 4" 3)
      (if-false "if zero?(1) then 3 else 4" 4)
      
      ;; test dynamic typechecking
      (no-bool-to-diff-1 "-(zero?(0),1)" error)
      (no-bool-to-diff-2 "-(1,zero?(0))" error)
      (no-int-to-if "if 1 then 2 else 3" error)
      
      ;; make sure that the test and both arms get evaluated
      ;; properly. 
      (if-eval-test-true "if zero?(-(11,11)) then 3 else 4" 3)
      (if-eval-test-false "if zero?(-(11, 12)) then 3 else 4" 4)
      
      ;; and make sure the other arm doesn't get evaluated.
      (if-eval-test-true-2 "if zero?(-(11, 11)) then 3 else foo" 3)
      (if-eval-test-false-2 "if zero?(-(11,12)) then foo else 4" 4)
      
      ;; simple let
      (simple-let-1 "let x = 3 in x" 3)
      
      ;; make sure the body and rhs get evaluated
      (eval-let-body "let x = 3 in -(x,1)" 2)
      (eval-let-rhs "let x = -(4,1) in -(x,1)" 2)
      
      ;; check nested let and shadowing
      (simple-nested-let "let x = 3 in let y = 4 in -(x,y)" -1)
      (check-shadowing-in-body "let x = 3 in let x = 4 in x" 4)
      (check-shadowing-in-rhs "let x = 3 in let x = -(x,1) in x" 2)
      
      ;; test minus operate
      (just-minus "minus(-1)" 1)
      (book-exam "minus(-(minus(5), 9))" 14)
      (var-in-minus "minus(-(minus(x), 9))" 19)
      
      ;; test add operate
      (just-add "+(10, 2)" 12)
      
      ;; test mulit operate
      (just-mulit "*(10, 2)" 20)
      
      ;; test quotient  operate
      (just-quotient "quotient(10,3)" 3)
      
      ;; test equal? greater? less?
      (no-equal "equal?(1, 2)" #f)
      (equal "equal?(10, 10)"  #t)
      (greater "greater?(10, 2)" #t)
      (no-greater "greater?(19, 19)" #f)
      (less "less?(4,6)" #t)
      (no-less "less?(88,7)" #f)
      
      ;; list test
      ;; '() lead to wrong because ' is at first line of test-list
      (empty-list "emptylist" ())
      (one-item "cons(1,emptylist)" (1))
      (two-item "cons(1,cons(zero?(0),emptylist))" (1 #t))
      (list-in-item "cons(1,cons(zero?(0),cons(cons(10,emptylist),emptylist)))" (1 #t (10)))
      
      ;; list operate
      (empty-list2 "list()" ())
      (one-item2 "list(100)" (100))
      (three-item "list(120, 200, zero?(0))" (120 200 #t))
      (e3.10 "let x=4 in list(x, -(x,1), -(x,3))" (4 3 1))
      
      ;cond operate
      (simple-cond "cond zero?(1) ==> 1 zero?(0) ==> 2 end" 2)
      (two-true-cond "cond zero?(0) ==> 1 zero?(0) ==> 2 equal?(10,1) ==> 3 end" 1)
      ;;(cond-in-let "let t = zero?(0) in let z = zero?(1) in cond {t ==> 1} {z ==> 2} end" 1)
      
      ; print operate
      (printf "print(cons(1,cons(zero?(0),cons(cons(10,emptylist),emptylist))))" 1)
      
      ; mult let
      (mult-let "
let x = 30
in let x = -(x,1)
       y = -(x,2)
   in -(x,y)" 1)
      
      ;; let*
      (simple-let* "
let x = 30
in let* x = -(x, 1) y = -(x,2)
   in -(x,y)" 2)
      
      ;; unpack
      (simple-unpack "
let u = 7
in unpack x y = cons(u, cons(3,emptylist))
   in -(x,y)" 4)
      ))
  
  (run-all)
  ;; pdf 97
  )





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
      (value-of-program 
       (translation-of-program (scan&parse string)))))
  
  (define translation-run
    (lambda (string) 
       (translation-of-program (scan&parse string))))
  
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
       ("equal?" "(" expression "," expression ")")
       equal?-exp)
      
      (expression 
       ("greater?" "(" expression "," expression ")")
       greater?-exp)
      
      (expression
       ("less?" "(" expression "," expression ")")
       less?-exp)
      
      (expression
       ("zero?" "(" expression ")") 
       zero?-exp)
      
      (expression 
       ("minus" "(" expression ")") 
       minus-exp)
      
      (expression
       ("if" expression "then" expression "else" expression)
       if-exp)
        
      (expression
       ("let" (arbno identifier "=" expression) "in" expression)
       let-exp) 
      
     ; (expression 
     ;  ("letrec" (arbno identifier "(" (separated-list identifier ",") ")" "=" expression ) "in" expression)
     ;  letrec-exp)
            
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
       ("proc" "(" (separated-list identifier ",") ")" expression)
       proc-exp)
      
     ; (expression
     ;  ("traceproc" "(" (separated-list identifier ",") ")" expression)
     ;  traceproc-exp)
            
      (expression
       ("(" expression (arbno expression) ")")
       call-exp)
      
      (expression
       ("cond"  (arbno "{" expression "==>" expression "}") "end")
       cond-exp)
      
      ;; nameless-expression for translate
      (expression
       ("%lexref" number) 
       nameless-var-exp)
      
      (expression
       ("%let" (arbno expression) "in" expression)
       nameless-let-exp)
      
      (expression
       ("%proc" expression)
       nameless-proc-exp)
      
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
  
  ;; traslation program to nameslass
  (define translation-of-program
    (lambda (exp)
      (cases program exp
        (a-program (program-exp) (a-program (translation-of program-exp (init-senv)))))))
  
  (define translation-of
    (lambda (exp env)
      (cases expression exp
        ;; just return
        (const-exp (exp1) exp)
        
        (emptylist-exp () exp)
        
        ; call sub exp and build exp
        (diff-exp (exp1 exp2) 
                  (diff-exp (translation-of exp1 env) 
                            (translation-of exp2 env)))
        
        (add-exp (exp1 exp2) 
                 (add-exp (translation-of exp1 env) 
                          (translation-of exp2 env)))
        
        (mulit-exp (exp1 exp2) 
                   (mulit-exp (translation-of exp1 env) 
                              (translation-of exp2 env)))
        
        (quotient-exp (exp1 exp2) 
                      (quotient-exp (translation-of exp1 env) 
                                   (translation-of exp2 env)))
        
        (equal?-exp (exp1 exp2) 
                    (equal?-exp (translation-of exp1 env) 
                                (translation-of exp2 env)))
        
        (greater?-exp (exp1 exp2) 
                      (greater?-exp (translation-of exp1 env) 
                                    (translation-of exp2 env)))
        
        (less?-exp (exp1 exp2) 
                   (less?-exp (translation-of exp1 env) 
                              (translation-of exp2 env)))
       
        (if-exp (exp1 exp2 exp3)
                (if-exp (translation-of exp1 env) 
                        (translation-of exp2 env)
                        (translation-of exp3 env)))
        
        (zero?-exp (exp1)
                   (zero?-exp (translation-of exp1 env)))
        
        (minus-exp (exp1)
                   (minus-exp (translation-of exp1 env)))
        
        
        (car-exp (exp1) 
                 (car-exp (translation-of exp1 env)))
        
        (cdr-exp (exp1)
                 (cdr-exp (translation-of exp1 env)))
        
        (list-exp (exps)
                  (list-exp (translation-of-exps exps env)))
        
        (cons-exp (exp1 exp2)
                  (cons-exp (translation-of exp1 env)
                            (translation-of exp2 env)))
        
        (call-exp (rator rands) 
                  (call-exp (translation-of rator env)
                            (translation-of-exps rands env)))
        
        (cond-exp (exps1 exps2)
                  (cond-exp (translation-of-exps exps1 env)
                            (translation-of-exps exps2 env)))
        
        ;; translate to nameless exp
        (var-exp (exp1)
                 (nameless-var-exp (apply-senv env exp1)))
        
        (let-exp (vars exps body)
                 (let ((new-env (extend-vars-in-senv vars env)))
                   ; let var val use old-env to resolve
                   (nameless-let-exp (translation-of-exps exps env) 
                                     (translation-of body new-env))))
                  
        
        (proc-exp (vars body)
                  (let ((new-env (extend-vars-in-senv vars env)))
                    (nameless-proc-exp (translation-of body new-env))))
        
        ;(traceproc-exp (vars body) (proc-val (procedure vars body env #t)))
        
        ;(letrec-exp (p-names p-varss p-bodys letrec-body) (value-of-letrec p-names p-varss p-bodys letrec-body env))
        
        
        ;; source code can not have nameless exp
        (else
         (eopl:printf 'translation-of "invalid source expression ~s" exp))
        
        )))
  
  (define extend-vars-in-senv
    (lambda (vars senv)
      (if (null? vars)
          senv
          (extend-senv (car vars) (extend-vars-in-senv (cdr vars) senv)))))
  
  (define translation-of-exps
    (lambda (exps env)
      (if (null? exps)
          '()
          (cons (translation-of (car exps) env)
                (translation-of-exps (cdr exps) env)))))
  
  (define value-of
    (lambda (exp env)
      (cases expression exp
        ;; nameless exp
        (nameless-var-exp (num) (apply-env env num))
        
        (nameless-let-exp (exps body) (value-of-let exps body env))
        
        (nameless-proc-exp (body) (proc-val (procedure body env #f)))
        
        (call-exp (rator rands) (value-of-call-exp rator rands env))
        
        ;; just as old
        (const-exp (exp1) (num-val exp1))
        
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
                
        (if-exp (exp1 exp2 exp3) (if (expval->bool (value-of exp1 env)) 
                                     (value-of exp2 env)
                                     (value-of exp3 env)))
        
        (zero?-exp (exp1) (bool-val (= (expval->num (value-of exp1 env)) 0)))
        
        (minus-exp (exp1) (num-val (- 0 (expval->num (value-of exp1 env)))))
                
        (emptylist-exp () empty-list-val)
        
        (car-exp (exp1) (list-val (car (expval->list (value-of exp1 env)))))
        
        (cdr-exp (exp1) (list-val (cdr (expval->list (value-of exp1 env)))))
        
        (list-exp (exps) (list-val (value-of-exps exps env)))
        
        (cons-exp (exp1 exp2) (list-val (cons (value-of exp1 env)
                                              (expval->list (value-of exp2 env)))))
        
        (cond-exp (exps1 exps2) (value-of-cond exps1 exps2 env))
        
        ;(traceproc-exp (vars body) (proc-val (procedure vars body env #t)))
        
        
        ;(letrec-exp (p-names p-varss p-bodys letrec-body) (value-of-letrec p-names p-varss p-bodys letrec-body env))
        
        ;; translation code can not have name exp
        (else
         (eopl:printf 'value-of "invalid translation expression ~s" exp))
        )))
  
  (define value-of-cond
    (lambda (conds results env)
      (if (null? conds)
       (eopl:error 'value-of-cond "no one branch is true")
       (if (expval->bool (value-of (car conds) env))
           (value-of (car results) env)
           (value-of-cond (cdr conds) (cdr results) env)))))  
  
  (define value-of-let
    (lambda (exps body env)
      (let ((vals (value-of-exps exps env)))
        (value-of body (extend-vals-in-env vals env)))))
  
  (define extend-vals-in-env
    (lambda (vals env)
      (if (null? vals)
          env
          (extend-env (car vals) 
                      (extend-vals-in-env (cdr vals) env)))))
   
  (define value-of-exps
    (lambda (exps env)
      (if (null? exps)
          '()
          (cons (value-of (car exps) env)
                (value-of-exps (cdr exps) env)))))
  
  
  
  ;; value-of func first and find out if need trace and value-of args
  (define value-of-call-exp
    (lambda (rator rands env)
      (letrec ((func (expval->proc (value-of rator env)))
               (trace (is-proc-trace func)))
        (let () 
          (if trace
              (eopl:printf "traceproc on entry~%") #t)
          
          (letrec ((args (value-of-exps rands env))
                   (ret (apply-procedure func args)))
            (if trace
                (eopl:printf "traceproc on exit~%") #t)
            ret)))))
  
  (define is-proc-trace
    (lambda (func)
      (cases proc func
        (procedure (body env trace)
                   trace))))
  
  (define apply-procedure
    (lambda (func vals)
      (cases proc func
        (procedure (body env trace)
                   (value-of body (extend-vals-in-env vals env))))))
  
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
      ; (if-eval-test-true-2 "if zero?(-(11, 11)) then 3 else foo" 3)
      ; (if-eval-test-false-2 "if zero?(-(11,12)) then foo else 4" 4)
      
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
      
      ;; proc test
      (proc1 "let f = proc (x) -(x,11) in (f(f 77))" 55)
      (proc2 "(proc (f) (f (f 77)) proc (x) -(x, 11))" 55)
      (lexical-scope "
let x = 200 
   in let f = proc (z) -(z,x) 
      in let x = 100 
         in let g = proc (z) -(z,x)
            in -((f 1), (g 1))" -100)
      
      ;; simple applications
      (apply-proc-in-rator-pos "(proc(x) -(x,1)  30)" 29)
      (apply-simple-proc "let f = proc (x) -(x,1) in (f 30)" 29)
      (let-to-proc-1 "(proc(f)(f 30)  proc(x)-(x,1))" 29)
      
      
      (nested-procs "((proc (x) proc (y) -(x,y)  5) 6)" -1)
      (nested-procs2 "let f = proc(x) proc (y) -(x,y) in ((f -(10,5)) 6)"
                     -1)
      
      (y-combinator-1 "
let fix =  proc (f)
            let d = proc (x) proc (z) ((f (x x)) z)
            in proc (n) ((f (d d)) n)
in let
    t4m = proc (f) proc(x) if zero?(x) then 0 else -((f -(x,1)),-4)
in let times4 = (fix t4m)
   in (times4 4)" 16)
            
      ;; e3.20
      (currying-add-1 "let add = proc(x) proc (y) -(x, -(0,y)) in ((add 1) 2)" 3)
      (currying-add-2 "let add = proc(x) proc (y) -(x, -(0,y)) in ((add 11) 2)" 13)
      (currying-add-3 "let add = proc(x) proc (y) -(x, -(0,y)) in ((add -1) 2)" 1)
      
      ;; multi args
      (mulit-args-add "let add = proc(x,y) -(x,-(0,y)) in (add 12 2)" 14)
      (mulit-args-add "let add = proc(x,y,z) -(-(x,-(0,y)),-(0,z)) in (add 12 2 3)" 17)
      
      ;; e3.23
      (factorial "
let makefact = proc (maker, x)
                 if zero?(x)
                 then 1
                 else *(x, (maker maker -(x,1)))
in let fact = proc (x) (makefact makefact x)
   in (fact 5)" 120)
      
      ;; e3.24
      (odd-even "
let makeodd =
proc (m-even, m-odd)
  proc (x)
    if zero?(x)
    then zero?(1)
    else ((m-even m-odd m-even) -(x,1))
in let makeeven =
   proc (m-odd, m-even)
     proc (x)
       if zero?(x)
       then zero?(0)
       else ((m-odd m-even m-odd) -(x,1))
in let odd = proc (x) ((makeodd makeeven makeodd) x)
in let even = proc (x) ((makeeven makeodd makeeven) x)
in (odd 10)" #f)
      
      ;; e3.25
      (y-combinator-2 "
let makerec =
  proc(f)
    let d = 
      proc(d)
        proc(x)
          (f (d d) x)
      in proc(x) ((d d) x)
in let time4rec = 
     proc(f, x)
     if zero?(x)
       then 0
       else +(4, (f -(x,1)))
in let time4 = (makerec time4rec)
in (time4 10)" 40)
      
         
     
     ;; e3.37
     (lexical-recursion "
let fact = proc (n) +(n, 1)
in let fact = proc (n)
               if zero?(n)
               then 1
               else *(n, (fact -(n,1)))
   in (fact 5)" 25)
     
     ;cond operate e3.38
     (simple-cond "cond{zero?(1) ==> 1} {zero?(0) ==> 2} end" 2)
     (two-true-cond "cond{zero?(0) ==> 1} {zero?(0) ==> 2} {equal?(10,1) ==> 3} end" 1)
     (cond-in-let "let t = zero?(0) in let z = zero?(1) in cond {t ==> 1} {z ==> 2} end" 1)
      
     ))
  
  (run-all)
  
  ;; pdf 116
  )





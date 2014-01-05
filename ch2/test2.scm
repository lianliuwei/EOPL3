(module sec2.5 (lib "eopl.ss" "eopl")
  
  (require "utils.scm")
  
  (define-datatype lc-exp lc-exp?
    (var-exp (var symbol?))
    (lambda-exp (bound-vars symbol?) (body lc-exp?))
    (apply-exp (rator lc-exp?) (rands lc-exp?)))
  
  (define occurs-free?
    (lambda (search-var exp)
      (cases lc-exp exp
        (var-exp (var) (eqv? search-var var))
        (lambda-exp (bound-vars body) 
                    (and (not (eqv? search-var bound-vars)) 
                         (occurs-free? search-var body)))
        (apply-exp (rator rands) 
                   (or (occurs-free? search-var rator)
                       (occurs-free? search-var rands))))))
  
  (equal?? (occurs-free? 'x (var-exp 'x)) #t)
  (equal?? (occurs-free? 'x (var-exp 'y)) #f)
  (equal?? (occurs-free? 'x (lambda-exp 'x (apply-exp (var-exp 'x) (var-exp 'y)))) #f)
  (equal?? (occurs-free? 'x (lambda-exp 'y (apply-exp (var-exp 'x) (var-exp 'y)))) #t)
  (equal?? (occurs-free? 'x (apply-exp (var-exp 'x) (var-exp 'y))) #t)
  (equal?? (occurs-free? 'x 
                         (lambda-exp 'y 
                                     (lambda-exp 'z 
                                                 (apply-exp 
                                                  (var-exp 'x) 
                                                  (apply-exp 
                                                   (var-exp 'y) 
                                                   (var-exp 'z)))))) 
           #t)
  
  (define-datatype s-exp s-exp?
    (symbol-s-exp (sym symbol?))
    (s-list-s-exp (slist s-list-exp?)))
  
  (define-datatype s-list-exp s-list-exp?
    (empty-s-list)
    (no-empty-s-list (first s-exp?) (rest s-list-exp?)))
  
  
  (no-empty-s-list (s-list-s-exp (no-empty-s-list (symbol-s-exp 'x) (empty-s-list))) (empty-s-list))
  
  (define-datatype s-exp2 s-exp2?
    (symbol-s-exp2 (sym symbol?))
    (s-list-s-exp2 (slist s-list-exp2?)))
  
  (define-datatype s-list-exp2 s-list-exp2?
    (a-s-list-exp2 (exps s-list?)))
  
  (define s-list?
    (lambda (val)
      (if (null? val)
          #t
          (if (pair? val)
              (and (s-exp2? (car val)) (s-list? (cdr val)))
              #f
              ))))
  
  
  (a-s-list-exp2 
   (list 
    (s-list-s-exp2 
     (a-s-list-exp2 (list (symbol-s-exp2 'x))))
    (symbol-s-exp2 'x)
    (symbol-s-exp2 'y)))
  
  (define parse-expression
    (lambda (datum)
      (cond
        ((symbol? datum) (var-exp datum))
        ((eqv? 'lambda (car datum)) (lambda-exp (caadr datum) (parse-expression (caddr datum))))
        (else (apply-exp (parse-expression (car datum)) (parse-expression (cadr datum))))
        )))
  
  (parse-expression '(lambda (x) (x y)))
  
  (equal?? (parse-expression '(lambda (x) (x y)))
           (lambda-exp 'x (apply-exp (var-exp 'x) (var-exp 'y))))
  
  (define unparse-lc-exp
    (lambda (exp)
      (cases lc-exp exp
        (var-exp (var) var)
        (lambda-exp (bound-var body) (list 'lambda (list bound-var) (unparse-lc-exp body)))
        (apply-exp (rator rands) (list (unparse-lc-exp rator) (unparse-lc-exp rands)))
        )))
  
  (equal?? (unparse-lc-exp (lambda-exp 'x (apply-exp (var-exp 'x) (var-exp 'y)))) '(lambda (x) (x y)))
  (equal?? (unparse-lc-exp (lambda-exp 'y 
                                     (lambda-exp 'z 
                                                 (apply-exp 
                                                  (var-exp 'x) 
                                                  (apply-exp 
                                                   (var-exp 'y) 
                                                   (var-exp 'z)))))) '(lambda (y) (lambda (z) (x (y z)))))
 )
(module sec2.5 (lib "eopl.ss" "eopl")
  
  (require "../lib/utils.scm")
  
  (define a-list?
    (lambda (pred?)
      (lambda (val)
        (if (null? val)
            #t
            (if (pair? val)
                (and (pred? (car val)) (a-list? (cdr val)))
                #f
                )))))
  
  (define-datatype lc-exp2 lc-exp2?
    (var-exp2 (var symbol?))
    (lambda-exp2 (bound-vars (a-list? symbol?)) (body lc-exp2?))
    (apply-exp2 (rator lc-exp2?) (rands (a-list? lc-exp2?)))
    )
  
  (define unparse-lc-exp2-rands
    (lambda (rands)
      (if (null? rands)
          '()
          (if (pair? rands)
              (cons (unparse-lc-exp2 (car rands)) (unparse-lc-exp2-rands (cdr rands)))
              eopl:error)
         )))
  
  (define unparse-lc-exp2
    (lambda (exp)
      (cases lc-exp2 exp
        (var-exp2 (var) var)
        (lambda-exp2 (bound-vars body) (list 'lambda bound-vars (unparse-lc-exp2 body)))
        (apply-exp2 (rator rands) (cons (unparse-lc-exp2 rator) (unparse-lc-exp2-rands rands)))
        )))
  
  (equal?? (unparse-lc-exp2 (lambda-exp2 (list 'x 'y) (apply-exp2 (var-exp2 'x) (list (var-exp2 'y))))) '(lambda (x y) (x y)))
  
  (equal?? (unparse-lc-exp2 (lambda-exp2 (list 'z 'k 'l) (lambda-exp2 (list 'x 'y) (apply-exp2 (var-exp2 'x) (list (var-exp2 'y) (var-exp2 'z) (apply-exp2 (var-exp2 'k) (list (var-exp2 'l))))))))
                            '(lambda (z k l) (lambda (x y) (x y z (k l)))))
  
  )
(module test1 mzscheme
  
  (require "../lib/utils.scm")
    
  (define in-s? 
    (lambda (n)
      (if (zero? n) #t
          (if (>= (- n 3) 0)
              (in-s? (- n 3))
              #f))))
  
  (equal?? (in-s? 0) #t)
  (equal?? (in-s? 10) #f)
  (equal?? (in-s? 9) #t)
  
  (define list-length
    (lambda (n) 
      (if (null? n) 0
          (+ 1 (list-length (cdr n))))))
  
  (equal?? (list-length '(a)) 1)
  (equal?? (list-length '(a b)) 2)
  (equal?? (list-length '(a b (c d))) 3)
  
  (define nth-element 
    (lambda (list n)
      (if (null? list)
          (report-list-too-short n)
          (if (= n 0) (car list)
              (nth-element (cdr list) (- n 1))))))
  
  (define report-list-too-short
    (lambda (n)
      (error 'nth-element 
             "list too short by ~s elements.~%" (+ n 1))))
  
  (equal?? (nth-element '(a) 0) 'a)
  (equal?? (nth-element '(1 2) 1) 2)
  (equal?? (nth-element '((1 2 3) 4 (5 6) (7 8)) 2) '(5 6))
  #(nth-element '(1 2 3) 3)
  
  (define remove-first
    (lambda (list n)
      (if (null? list)
          '()
          (if (eqv? n (car list))
              (cdr list)
              (cons (car list) (remove-first (cdr list) n))))))
  
  (equal?? (remove-first '(a b c) 'a) '(b c))
  (equal?? (remove-first '(a b c) 'd) '(a b c))
  (equal?? (remove-first '(a b c a) 'a) '(b c a))
  #(equal?? (remove-first '(a b c (d e)) '(d e)) '(a b c))
  #(equal?? (eqv? (car '((d e) a)) '(d e)) #t)
  #(equal?? (eqv? '(d e) '(d e)) #t)
  
  (define remove
    (lambda (list n)
      (if (null? list)
          '()
          (if (eqv? n (car list))
              (remove (cdr list) n)
              (cons (car list) (remove (cdr list) n))))))
  
  (equal?? (remove '(a b c a) 'a) '(b c))
  (equal?? (remove '(e b a c f a) 'a) '(e b c f))
  
  (define occurs-free?
    (lambda (var exp)
      (cond
        ((symbol? exp) (eqv? exp var))
        ((eqv? 'lambda (car exp)) 
         (if (eqv? (caadr exp) var)
             #f
             (occurs-free? var (caddr exp))))
        (else (or (occurs-free? var (car exp))
                  (occurs-free? var (cadr exp))))
        )))
  
  (equal?? (occurs-free? 'x 'x) #t)
  (equal?? (occurs-free? 'x 'y) #f)
  (equal?? (occurs-free? 'x '(lambda (x) (x y))) #f)
  (equal?? (occurs-free? 'x '(lambda (y) (x y))) #t)
  (equal?? (occurs-free? 'x '((x y))) #t)
  (equal?? (occurs-free? 'x '(lambda (y) (lambda (z) (x (y z))))) #t)
  
  
  (define subst
    (lambda (new old slist)
      (if (null? slist)
          '()
          (cons (subst-in-s-exp new old (car slist))
                (subst new old (cdr slist))))))
  
  (define subst-in-s-exp
    (lambda (new old slist)
      (if (symbol? slist)
          (if (eqv? slist old)
              new
              slist)
          (subst new old slist))))
  
  (equal?? (subst 'a 'b '((b c) (b () d))) '((a c) (a () d)))
  (equal?? (subst 'a 'c '((b c) (b () d))) '((b a) (b () d)))
  )


(module environments (lib "eopl.ss" "eopl") 
  
  ;; builds environment interface, using data structures defined in
  ;; data-structures.scm. 

  (require "data-structures.scm")

  (provide init-env empty-env extend-env apply-env init-senv empty-senv extend-senv apply-senv)

;;;;;;;;;;;;;;;; initial environment ;;;;;;;;;;;;;;;;
  
  ;; init-env : () -> Env
  ;; usage: (init-env) = [i=1, v=5, x=10]
  ;; (init-env) builds an environment in which i is bound to the
  ;; expressed value 1, v is bound to the expressed value 5, and x is
  ;; bound to the expressed value 10.
  ;; Page: 69
  (define init-env 
    (lambda ()
      (extend-env 
       (num-val 1)
       (extend-env
        (num-val 5)
        (extend-env
         (num-val 10)
         (empty-env))))))
  
  (define init-senv
    (lambda ()
      (extend-senv 
       'i 
       (extend-senv 
        'v 
        (extend-senv
         'x
         (empty-senv))))))

;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;

  (define empty-env
    (lambda ()
      '()))
  
  (define empty-env? 
    (lambda (x)
      (null? x)))

  (define extend-env
    (lambda (val old-env)
      (cons val old-env)))

  (define apply-env
    (lambda (env index)
      (list-ref env index)))

  ;; senv
  (define empty-senv
    (lambda ()
      '()))
  
  (define empty-senv?
    (lambda (senv)
      (null? senv)))
  
  (define extend-senv
    (lambda (sym old-senv)
      (cons sym old-senv)))
  
  (define apply-senv
    (lambda (senv search-sym)
      (if (empty-senv? senv)
          (eopl:error 'apply-senv "No binding for ~s" search-sym)
          (if (eqv? search-sym (car senv))
              0
              (+ 1 (apply-senv (cdr senv) search-sym))))))
  
  )

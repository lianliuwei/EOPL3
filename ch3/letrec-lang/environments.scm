(module environments (lib "eopl.ss" "eopl") 
  
  ;; builds environment interface, using data structures defined in
  ;; data-structures.scm. 

  (require "data-structures.scm")

  (provide init-env empty-env extend-env extend-env-rec apply-env)

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
       'i (num-val 1)
       (extend-env
        'v (num-val 5)
        (extend-env
         'x (num-val 10)
         (empty-env))))))

;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;

  (define empty-env
    (lambda ()
      (empty-env-record)))
  
  (define empty-env? 
    (lambda (x)
      (empty-env-record? x)))

  (define extend-env
    (lambda (sym val old-env)
      (extended-env-record sym val #f old-env)))

  (define extend-env-rec
    (lambda (sym val old-env)
      (extended-env-record sym val #t old-env)))
  
  (define apply-env
    (lambda (env search-sym)
      (if (empty-env? env)
	(eopl:error 'apply-env "No binding for ~s" search-sym)
	(let ((sym (extended-env-record->sym env))
	      (val (extended-env-record->val env))
              (rec (extended-env-record->rec env))
	      (old-env (extended-env-record->old-env env)))
          (if (and rec (eqv? search-sym sym))
              (cases expval val
                (proc-val (p) 
                          (cases proc p
                            (procedure (vars let-body save-env trac)
                                       (proc-val (procedure vars let-body (extend-env-rec sym val save-env) trac)))))
                (else (eopl:error 'apply-env "rec only for prc-val")))
	  (if (eqv? search-sym sym)
	    val
	    (apply-env old-env search-sym)))))))

  )

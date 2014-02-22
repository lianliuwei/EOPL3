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
      (extended-env-record sym val old-env)))

  (define extend-env-rec
    (lambda (p-names b-varss bodys old-env)
      (let ((new-env (extend-env-rec-helper1 p-names old-env)))
        (extend-env-rec-helper2 b-varss bodys new-env new-env)
        new-env)))
  
  (define extend-env-rec-helper1
    (lambda (p-names old-env)
      (let ((vec (make-vector 1)))
        (if (null? p-names)
            old-env
            (extend-env (car p-names) vec (extend-env-rec-helper1 (cdr p-names) old-env))))))
  
  (define extend-env-rec-helper2
    (lambda (b-varss bodys raw-env first-env)
      (if (null? b-varss)
          #t
          (let ((vec (extended-env-record->val raw-env))
                (old-env (extended-env-record->old-env raw-env)))
            (vector-set! vec 0
                           (proc-val (procedure (car b-varss) (car bodys) first-env #f)))
            (extend-env-rec-helper2 (cdr b-varss) (cdr bodys) old-env first-env)))))
  
  (define apply-env
    (lambda (env search-sym)
      (if (empty-env? env)
	(eopl:error 'apply-env "No binding for ~s" search-sym)
	(let ((sym (extended-env-record->sym env))
	      (val (extended-env-record->val env))
	      (old-env (extended-env-record->old-env env)))
	  (if (eqv? search-sym sym)
	    (if (vector? val)
                ;; return beforehand created procedure
                (vector-ref val 0)
                val)
	    (apply-env old-env search-sym))))))

  
  )

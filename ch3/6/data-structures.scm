(module data-structures (lib "eopl.ss" "eopl")
  
  ;; data structures for let-lang.
  
  (provide (all-defined-out))               ; too many things to list
  
  ;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;
  
  ;;; an expressed value is either a number, a boolean or a procval.
  
  (define list-of
    (lambda (pred)
      (lambda (val)
        (or (null? val)
            (and (pair? val)
                 (pred (car val))
                 ((list-of pred) (cdr val)))))))
  
  (define-datatype expval expval?
    (num-val
     (value number?))
    (bool-val
     (boolean boolean?))
    (list-val
     (list (list-of expval?))))
  
  (define empty-list-val
    (list-val '()))
  
  (define list->list-val
    (lambda (list) 
      (if (null? list)
          empty-list-val
          (list-val (list->list-val-help list)))))
  
  (define list->list-val-help
    (lambda (list)
      (if (null? list)
          '()
          (cons
           (cond
             ((number? (car list)) (num-val (car list)))
             ((boolean? (car list)) (bool-val (car list)))
             (list? (car list) (list->list-val (car list))))
           (list->list-val-help (cdr list))))))
  
  (define expval->val
    (lambda (exp)
      (cases expval exp
        (num-val (val) val)
        (bool-val (val) val)
        (list-val (val) (expval->list-help val))
        )))
  
  (define expval->list-help
    (lambda (list-exp)
      (if (null? list-exp)
          '()
          (cons (expval->val (car list-exp))
                (expval->list-help (cdr list-exp))))))
  
  ;;; extractors:
  
  ;; expval->num : ExpVal -> Int
  ;; Page: 70
  (define expval->num
    (lambda (v)
      (cases expval v
        (num-val (num) num)
        (else (expval-extractor-error 'num v)))))
  
  ;; expval->bool : ExpVal -> Bool
  ;; Page: 70
  (define expval->bool
    (lambda (v)
      (cases expval v
        (bool-val (bool) bool)
        (else (expval-extractor-error 'bool v)))))
  
  (define expval->list
    (lambda (v)
      (cases expval v
        (list-val (list) list)
        (else (expval-extractor-error 'list v)))))
  
  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                  variant value)))
  
  ;;;;;;;;;;;;;;;; environment structures ;;;;;;;;;;;;;;;;
  
  ;; example of a data type built without define-datatype
  
  (define empty-env-record
    (lambda () 
      '()))
  
  (define extended-env-record
    (lambda (sym val old-env)
      (cons (list sym val) old-env)))
  
  (define empty-env-record? null?)
  
  (define environment?
    (lambda (x)
      (or (empty-env-record? x)
          (and (pair? x)
               (symbol? (car (car x)))
               (expval? (cadr (car x)))
               (environment? (cdr x))))))
  
  (define extended-env-record->sym
    (lambda (r)
      (car (car r))))
  
  (define extended-env-record->val
    (lambda (r)
      (cadr (car r))))
  
  (define extended-env-record->old-env
    (lambda (r)
      (cdr r)))
  
  )

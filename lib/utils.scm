(module utils (lib "eopl.ss" "eopl")

  ;; a very simple macro for inline testing

  (provide equal?? report-unit-tests-completed)

  ;; simple-minded magic for tests
  (define-syntax equal??
    (syntax-rules ()
      ((_ x y)
       (let ((x^ x) (y^ y))
         (if (not (equal? x y))
           (eopl:error 'equal??
             "~s returned ~s, should have returned ~s~%" 'x x 'y)
             #t)))))

  
  (define report-unit-tests-completed
    (lambda (fn-name)
      (eopl:printf "unit tests completed: ~s~%" fn-name)))

)

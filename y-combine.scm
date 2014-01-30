#lang racket

(define fix
  (lambda (f)
    (let 
        ((d 
         (lambda (d)
           (lambda (n)
             ((f (d d)) n)))))
      (d d))))

(define t4m
  (lambda (f)
    (lambda (n)
      (if (= n 0)
          0
          (+ 4 (f (- n 1)))))))

((fix t4m) 5) ; 20
((fix t4m) 4) ; 16
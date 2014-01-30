#lang racket

(define fix
  (lambda (f)
    (let 
        ((d 
         (lambda (d)
           (lambda (n)
             ((f (d d)) n)))))
      (lambda (n)
        ((f (d d)) n)))))

(define t4m
  (lambda (f)
    (lambda (n)
      (if (= n 0)
          0
          (+ 4 (f (- n 1)))))))


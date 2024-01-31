#lang racket

(define all-but-last
  (lambda (lst)
    (if (null? (rest lst))
        '()
        (cons (first lst)
              (all-but-last (rest lst))))))
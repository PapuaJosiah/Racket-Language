;;
;; FILE:     homework06-tests.rkt
;; AUTHOR:   YOUR NAME
;; DATE:     YOUR DATE
;; COMMENT:  This file loads "homework06.rkt" and runs tests on its
;;           publicly-defined functions.
;;
;; MODIFIED: 
;; CHANGE:   
;;

#lang racket
(require rackunit)
(require "homework06.rkt")

;; --------------------------------------------------------------------------
;; Problem 1                                           (structural recursion)
;; --------------------------------------------------------------------------

(check-equal? (list-index '! '(a b c d ? ! e)) 5)
(check-equal? (list-index '6 '(a c d e 1 2 3 ! ^ 6)) 9)
(check-equal? (list-index 'a '(a)) 0)

;; --------------------------------------------------------------------------
;; Problem 2                                           (structural recursion)
;; --------------------------------------------------------------------------

(check-equal? (tree-sum '(1 2 3)) 6)
(check-equal? (tree-sum '(91 (104 109 110) 1)) 415)
(check-equal? (tree-sum '(1 (1 (1 1 1) (1 1 1)) (1 2 1))) 12)

;; --------------------------------------------------------------------------
;; Problem 3                                           (structural recursion)
;; --------------------------------------------------------------------------

(check-equal? (tree? '(1 (1 1 1) 1)) #t)
(check-equal? (tree? '(1 (1 (1 1 1) 1) 1)) #t)
(check-equal? (tree? '(1 (1 (1 a 1) 1) 1)) #f)

;; --------------------------------------------------------------------------
;; Problem 4                                                (little language)
;; --------------------------------------------------------------------------

(check-equal? (unused-var? 'x '(lambda (t) (lambda (x) y))) #t)
(check-equal? (unused-var? 't '(lambda (t) t)) #t)
(check-equal? (unused-var? 'x '(lambda (x) x)) #t)

;; --------------------------------------------------------------------------


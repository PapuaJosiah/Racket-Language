
;;
;; FILE:     homework06.rkt
;; AUTHOR:   Josiah Lewis
;; DATE:     03-02-2023
;; COMMENT:  This module defines the five functions specified in
;;           Homework 6 as an importable module.  Use the function
;;           stub if you do not have a working solution of your own.
;;
;; MODIFIED: 
;; CHANGE:   
;;CREDIT: REED

#lang racket
(require "syntax-procs.rkt")
(require "occurs-procs.rkt")
(provide list-index tree-sum tree? unused-var?)

;; --------------------------------------------------------------------------
;; Problem 1                                           (structural recursion)
;; --------------------------------------------------------------------------

(define list-index
  (lambda (s los)
    (list-index-interface s los 0)))


(define list-index-interface
  (lambda (s los position)
    (if (null? los)
        -1
        (if (equal? (first los) s)
            position
            (list-index-interface s (rest los) (add1 position))))))

;; --------------------------------------------------------------------------
;; Problem 2                                           (structural recursion)
;; --------------------------------------------------------------------------

(define tree-sum
  (lambda (bin-tree)
   (tree-sum-interface bin-tree 0)))

(define tree-sum-interface
  (lambda ( los count)
    (if (null? los)
        count
        (if (list? (first los))
            (+ (tree-sum-interface (first los) count) (tree-sum-interface (rest los) count))
            (+ (first los) (tree-sum-interface (rest los) count))))))
;; --------------------------------------------------------------------------
;; Problem 3                                           (structural recursion)
;; --------------------------------------------------------------------------

(define tree?
  (lambda (exp)
   (if (number? exp)
       #t
       (if (list? exp)
           (if (> (length exp) 3)
               #f
               (equal? (tree? (second exp)) (tree? (third exp))))
          #f))));if not a number nor a list of three numbers then it is not a tree
       
       

;; --------------------------------------------------------------------------
;; Problem 4                                                (little language)
;; --------------------------------------------------------------------------

(define unused-var?
  (lambda (v exp)
    (if (null? exp)
        #t
        (if (lambda? exp)
            (if (occurs-free? v exp)
                #f
                (unused-var? v (rest exp)))
            (if (lambda? (first exp))
                (unused-var? v (first exp))
                (unused-var? v (rest exp)))))))
        
    

;; --------------------------------------------------------------------------


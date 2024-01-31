;;  ------------------------------------------------------------------------
;; |   FILE           :  homework07.rkt                                     |
;; |   AUTHOR         :  Josiah Lewis                                       |
;; |   CREATION DATE  :  03/21/2023                                         |
;; |   DESCRIPTION    :  I dunno what im doing                              |
;;  ------------------------------------------------------------------------

#lang racket
(require "syntax-procs.rkt")
(require "language-procs.rkt")
(provide curry
         empty-set   set-empty?   set-member?   set-add
         set-union
         set-subset?
         free-vars
)

;; --------------------------------------------------------------------------
;; Problem 1                                           (structural recursion)
;; --------------------------------------------------------------------------

(define curry
  (lambda (lambda-exp)
    (helper (second lambda-exp) (lambda->body lambda-exp))))

(define helper
  (lambda (parameter body)
    (if (equal? (length parameter) 1)
        (list 'lambda parameter body)
        (list 'lambda (list (first parameter)) (helper (rest parameter) body)))))

;; --------------------------------------------------------------------------
;; Problem 2                                        (non-recursive solutions)
;; --------------------------------------------------------------------------

(define empty-set
  (lambda ()
    '()))

(define set-empty?
  (lambda (S)
    (if (empty? S)
        #t
        #f)))

(define set-member?
  (lambda (sym S)
    (if (equal? (member sym S) #f)
        #f
        #t)))

(define set-add
  (lambda (sym S)
    (if (set-member? sym S)
        S
        (cons sym S))))

;; --------------------------------------------------------------------------
;; Problem 3                                           (structural recursion)
;; --------------------------------------------------------------------------

(define set-union
  (lambda (S1 S2)
    (if (equal? (length S1) 0)
        S2
        (set-union (rest S1) (set-add (first S1) S2)))))

;; --------------------------------------------------------------------------
;; Problem 4                                           (structural recursion)
;; --------------------------------------------------------------------------


(define set-subset?
  (lambda (S1 S2)
    (if (equal? (length S1) 0)
        #t
        (and (set-member? (first S1) S2) (set-subset? (rest S1) S2)))))
          

;; --------------------------------------------------------------------------
;; Problem 5                                           (structural recursion)
;; --------------------------------------------------------------------------

(define free-vars
  (lambda (exp)
    (cond ((lambda? exp)
           (remove (lambda->param exp) (free-vars (lambda->body exp))))
           ((app? exp)
               (set-union (free-vars (app->proc exp)) (free-vars (app->arg exp))))
           (else (set-add exp (empty-set))))))
            
           

;; --------------------------------------------------------------------------

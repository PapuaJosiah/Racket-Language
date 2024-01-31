;;  ------------------------------------------------------------------------
;; |   FILE           :  syntax-procs.rkt                                   |
;; |   AUTHOR         :  Eugene Wallingford                                 |
;; |   CREATION DATE  :  2020/02/26                                         |
;; |   DESCRIPTION    :  These functions implement syntax procedures for    |
;; |                     a simple language grammar consisting only of       |
;; |                     strings and various string operations.             |
;; |                     It extends the language with variable references.  |
;;  ------------------------------------------------------------------------

#lang racket
(provide exp?
         varref?   make-varref   ; *NEW*
         str?      make-str
         unary?    make-unary    unary->op    unary->arg
         binary?   make-binary   binary->op   binary->left   binary->right
         mixed?    make-mixed    mixed->op    mixed->str     mixed->num)

;;   This code works with the following grammar:
;;
;;             <exp> ::= <string>
;;                     | <varref>                      ; *NEW*
;;                     | ( <unary-op> <exp> )
;;                     | ( <exp> <binary-op> <exp> )
;;                     | ( <exp> <mixed-op> <number> )
;;
;;        <unary-op> ::= rot13
;;       <binary-op> ::= +
;;        <mixed-op> ::= take | drop

;; type predicate for language expressions

(define exp?
  (lambda (exp)
    (or (str? exp)
        (varref? exp)   ; *NEW*
        (unary? exp)
        (binary? exp)
        (mixed? exp))))

;; strings

(define str? string?)

(define make-str identity)

;; varrefs                      ; *NEW*

(define varref? symbol?)        ; *NEW*

(define make-varref identity)   ; *NEW*

;; unary expressions

(define unary?
  (lambda (exp)
    (and (list? exp)
         (= (length exp) 2)
         (unary-op? (first exp))
         (exp? (second exp)))))

(define unary->op first)
(define unary->arg second)

(define make-unary
  (lambda (op arg)
    (list op arg)))

(define unary-op?
  (lambda (sym)
    (member sym '(rot13))))

;; binary expressions

(define binary?
  (lambda (exp)
    (and (list? exp)
         (= (length exp) 3)
         (exp? (first exp))
         (binary-op? (second exp))
         (exp? (third exp)))))

(define binary->op second)
(define binary->left first)
(define binary->right  third)

(define make-binary
  (lambda (op left right)
    (list left op right)))

(define binary-op?
  (lambda (sym)
    (member sym '(+))))

;; mixed expressions

(define mixed?
  (lambda (exp)
    (and (list? exp)
         (= (length exp) 3)
         (exp? (first exp))
         (mixed-op? (second exp))
         (number? (third exp)))))

(define mixed->op second)
(define mixed->str first)
(define mixed->num third)

(define make-mixed
  (lambda (op left right)
    (list left op right)))

(define mixed-op?
  (lambda (sym)
    (member sym '(take drop))))

;; ----- END OF FILE -----
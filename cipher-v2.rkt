;;  ------------------------------------------------------------------------
;; |   FILE           :  cipher-v2.rkt                                      |
;; |   AUTHOR         :  Eugene Wallingford                                 |
;; |   CREATION DATE  :  2020/02/26                                         |
;; |   DESCRIPTION    :  This file implements an interpreter for a simple   |
;; |                     language consisting of strings and various string  |
;; |                     operations, and a REPL that uses the interpreter.  |
;; |                     It extends the language with variable references.  |
;;  ------------------------------------------------------------------------

#lang racket
(require "cipher-syntax-procs.rkt")
(provide value interpret)

;; shift function ------------------------------------------------------
;
;(define shift
;  (lambda (str num)
;    (string-append (substring str num (string-length str))
;                   (substring str 0 num))))

;; value ---------------------------------------------------------------

(define *base-env* '((FIRST . "eugene")             ; *NEW*
                     (LAST  . "wallingford")))

(define value                                       ; *NEW*
  (lambda (exp)                                     ; interface
    (if (exp? exp)                                  ; procedure
        (value-aux exp *base-env*)
        (error 'value "invalid expression: ~a" exp))))

(define value-aux
  (lambda (exp env)                                 ; *NEW*
    (cond ((str? exp) exp)
          ((varref? exp) (lookup exp env))          ; *NEW*
          ((unary? exp) (eval-unary (unary->op exp)
                                    (value-aux (unary->arg exp) env)))
          ((binary? exp) (eval-binary (binary->op exp)
                                      (value-aux (binary->left exp) env)
                                      (value-aux (binary->right exp) env)))
          ((mixed? exp) (eval-mixed (mixed->op exp)
                                    (value-aux (mixed->str exp) env)
                                    (mixed->num exp)))
          (else (error 'value-aux "invalid expression: ~a" exp)))))

(define lookup                                      ; *NEW*
  (lambda (var env)
    (if (assoc var env)
        (cdr (assoc var env))
        (error 'lookup "invalid variable reference ~a" var))))

(define eval-unary
  (lambda (op arg)
    (cond ((eq? op 'rot13) (rot13 arg))
          (else (error 'eval-unary "invalid operator ~a" op)))))

(define eval-binary
  (lambda (op left right)
    (cond ((eq? op '+) (string-append left right))
          (else (error 'eval-binary "invalid operator ~a" op)))))

(define eval-mixed
  (lambda (op str num)
    (cond ((eq? op 'take) (substring str 0 num))
          ((eq? op 'drop) (substring str num (string-length str)))
          (else (error 'eval-mixed "invalid operator ~a" op)))))

;; interpret -----------------------------------------------------------

(define interpret
  (lambda ()
    (displayln (value (read)))
    (interpret)))

;; rot13 ---------------------------------------------------------------

(define rot13
  (lambda (str)
    (list->string (map (lambda (c)
                         (cond ((a-to-m? c) (rot-ch c 13))
                               ((n-to-z? c) (rot-ch c -13))
                               (else c)))
                       (string->list str)))))

(define a-to-m?
  (lambda (c)
    (and (char-ci<=? #\a c)
         (char-ci<=? c #\m))))

(define n-to-z?
  (lambda (c)
    (and (char-ci<=? #\n c)
         (char-ci<=? c #\z))))

(define rot-ch
  (lambda (c n)
    (integer->char (+ n (char->integer c)))))

; end of file
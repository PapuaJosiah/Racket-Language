;;  ------------------------------------------------------------------------
;; |   FILE           :  cipher-v1.rkt                                      |
;; |   AUTHOR         :  Eugene Wallingford                                 |
;; |   CREATION DATE  :  2020/02/26                                         |
;; |   DESCRIPTION    :  This file implements an interpreter for a simple   |
;; |                     language consisting of strings and various string  |
;; |                     operations, and a REPL that uses the interpreter.  |
;;  ------------------------------------------------------------------------

#lang racket
(require "cipher-syntax-procs.rkt")
(provide value interpret)

;; value ---------------------------------------------------------------

(define value
  (lambda (exp)
    (cond ((str? exp) exp)
          ((unary? exp) (eval-unary (unary->op exp)
                                    (value (unary->arg exp))))
          ((binary? exp) (eval-binary (binary->op exp)
                                      (value (binary->left exp))
                                      (value (binary->right exp))))
          ((mixed? exp) (eval-mixed (mixed->op exp)
                                    (value (mixed->str exp))
                                    (mixed->num exp)))
          (else (error 'value "invalid expression: ~a" exp)))))

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

;; interpret: a REPL ---------------------------------------------------

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
  (lambda (ch)
    (and (char-ci<=?  #\a  ch)
         (char-ci<=?  ch   #\m))))

(define n-to-z?
  (lambda (ch)
    (char-ci<=?  #\n  ch  #\z)))

(define rot-ch
  (lambda (c n)
    (integer->char (+ n (char->integer c)))))

; end of file
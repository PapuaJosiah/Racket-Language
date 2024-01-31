;;  ------------------------------------------------------------------------
;; |                                                                        |
;; |   FILE           :  wallingf-tests.rkt                                 |
;; |   AUTHOR         :  Eugene Wallingford                                 |
;; |                                                                        |
;; |   DESCRIPTION    :  Tests for solutions to Homework 9.                 |
;; |                     Not exhaustive, especially on integer operators.   |
;; |                                                                        |
;;  ------------------------------------------------------------------------

#lang racket
(require rackunit)
(require "interpreter.rkt")

;; --------------------------------------------------------------------------
;; Problem 2: preprocess
;; --------------------------------------------------------------------------

(check-equal? (preprocess '(3 @ 9))
              '((3 + 9) / 2)
              "average")
(check-equal? (preprocess '(- (3 @ 9)))
              '(- ((3 + 9) / 2))
              "embedded average")
(check-equal? (preprocess '((2 * 14) + (13 @ 29)))
              '((2 * 14) + ((13 + 29) / 2))
              "deeply embedded average")
(check-equal? (preprocess 1)
              1
              "number")
(check-equal? (preprocess '(- 4))
              '(- 4)
              "unary, plain")
(check-equal? (preprocess '(2 + 4))
              '(2 + 4)
              "binary, plain")
(check-equal? (preprocess '((2 * 14) + (4 - 6)))
              '((2 * 14) + (4 - 6))
              "compound, plain")

;; --------------------------------------------------------------------------
;; Problem 3: eval-exp
;; --------------------------------------------------------------------------

(check-equal? (eval-exp 0)
              0
              "number")
(check-equal? (eval-exp '(- 2))
              -2
              "core unary exp")
(check-equal? (eval-exp '(2 + 4))
              6
              "core binary exp")
(check-equal? (eval-exp '(4 * (12 - 7)))
              20
              "core compound exp")
(check-equal? (eval-exp '(2 @ 6))
              4
              "abstract binary exp")
(check-equal? (eval-exp '(4 * (12 @ 8)))
              40
              "abstract compound exp")
(check-equal? (eval-exp '((6 @ 10) * (12 @ 8)))
              80
              "two abstractions, nested")
(check-equal? (eval-exp '(((130 - 2) / 4) @ (15 / (17 % (sq 4)))))
              23
              "an exp with everything")

;; --------------------------------------------------------------------------
;; check exceptions on illegal expressions

(check-exn    exn:fail?
              (lambda () (eval-exp '(1 & 0)))
              "throws error for invalid expression: bad operator")
(check-exn    exn:fail?
              (lambda () (eval-exp '(a + 3)))
              "throws error for invalid expression: bad number")

;; --------------------------------------------------------------------------


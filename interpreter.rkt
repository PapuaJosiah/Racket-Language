;;  ------------------------------------------------------------------------
;; |                                                                        |
;; |   FILE           :  interpreter.rkt                                    |
;; |   AUTHOR         :  Eugene Wallingford                                 |
;; |                                                                        |
;; |   DESCRIPTION    :  Implements two Boom language processors as         |
;; |                     defined in Homework 9: preprocess and eval-exp.    |
;; |                     The evaluator uses several helper functions to     |
;; |                     do its job.                                        |
;; |                                                                        |
;;  ------------------------------------------------------------------------

#lang racket
(require "utilities.rkt")
(require "syntax-procs.rkt")

(provide eval-exp preprocess)

;; --------------------------------------------------------------------------
;; preprocessor
;; --------------------------------------------------------------------------

(define preprocess 
  (lambda (exp)
    (cond ; -------------------------- syntactic abstractions
          ((and (binary-exp? exp)
                (eq? '@ (binary-exp->operator exp)))
             (let ((left  (preprocess (binary-exp->left-op  exp)))
                   (right (preprocess (binary-exp->right-op exp))))
               (binary-exp (binary-exp left '+ right)
                           '/
                           (boom-number 2))))
          ((and (unary-exp? exp)
                (eq? 'sq (unary-exp->operator exp)))
             (let ((operand (preprocess (unary-exp->operand exp))))
               (binary-exp operand '* operand)))
          ; -------------------------- core features
          ((boom-number? exp) exp)
          ((unary-exp? exp)
             (unary-exp (unary-exp->operator exp)
                        (preprocess (unary-exp->operand exp))))
          ((binary-exp? exp)
             (binary-exp (preprocess (binary-exp->left-op exp))
                         (binary-exp->operator exp)
                         (preprocess (binary-exp->right-op exp))))
          (else (error 'preprocess "unreachable state ~a" exp)) )))

;; --------------------------------------------------------------------------
;; evaluator
;; --------------------------------------------------------------------------

(define eval-exp
  (lambda (exp)
    (if (boom-exp? exp)
        (eval-boom-exp (preprocess exp))
        (error 'eval-exp "illegal expression ~a" exp))))

(define eval-boom-exp
  (lambda (exp)
    (cond ( (boom-number? exp)
              (number->value exp) )
          ( (unary-exp? exp)
              (eval-unary-op
                 (unary-exp->operator exp)
                 (eval-boom-exp (unary-exp->operand exp))) )
          ( (binary-exp? exp)
              (eval-binary-op
                 (binary-exp->operator exp)
                 (eval-boom-exp (binary-exp->left-op  exp))
                 (eval-boom-exp (binary-exp->right-op exp))) )
          (else (error 'eval-exp "unreachable state ~a" exp)) )))

(define eval-unary-op
  (lambda (op-code arg)
    (cond ( (eq? op-code '- ) (- arg) )
          ( else (error 'eval-unary-op "illegal operator ~a" op-code)))))

(define eval-binary-op
  (lambda (op-code arg1 arg2)
    (cond ( (eq? op-code '+) (+ arg1 arg2) )
          ( (eq? op-code '-) (- arg1 arg2) )
          ( (eq? op-code '*) (* arg1 arg2) )
          ( (eq? op-code '/) (quotient  arg1 arg2) )
          ( (eq? op-code '%) (remainder arg1 arg2) )
          ( else (error 'eval-binary-op "illegal operator ~a" op-code) ))))

;; --------------------------------------------------------------------------


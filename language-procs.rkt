;;  ------------------------------------------------------------------------
;; |   FILE           :  language-procs.rkt                                 |
;; |   AUTHOR         :  Eugene Wallingford                                 |
;; |   CREATION DATE  :  2022/03/10                                         |
;; |   DESCRIPTION    :  This file includes various functions we have       |
;; |                     written that process programs in the little        |
;; |                     language: occurs-bound? and occurs-free? from      |
;; |                     Session 13, and the preprocessor from Session 16.  |
;;  ------------------------------------------------------------------------

#lang racket
(require "syntax-procs.rkt")
(provide preprocess occurs-bound? occurs-free?)

;; -------------------------------------------------------------------------
;;   These functions work with this version of the little language:
;;
;;                       --------------------------- CORE FEATURES
;;        <exp>      ::= <varref>
;;                     | ( lambda ( <var> ) <exp> )
;;                     | ( <exp> <exp> )
;;                       ---------------------------  ABSTRACTIONS
;;                     | ( let (<var> <exp>) <exp> )
;; -------------------------------------------------------------------------

;; -------------------------------------------------------------------------
;; preprocess translates expressions using the full language into
;; expressions using only the core language.
;; -------------------------------------------------------------------------

(define preprocess
  (lambda (exp)
    (cond
      ; ------------------------------------------- abstractions
      ( (let? exp) 
           (let ((var  (let->var  exp))
                 (val  (let->val  exp))
                 (body (let->body exp)))
             (make-app (make-lambda var (preprocess body))
                       (preprocess val)) ) )
      ; ------------------------------------------ core features
      ( (varref? exp)
           (make-varref exp) )
      ( (lambda? exp)
           (make-lambda (lambda->param exp)
                        (preprocess (lambda->body exp))) )
      ( (app? exp)
           (make-app (preprocess (app->proc exp))
                     (preprocess (app->arg  exp))) )
      ; -------------------------------------------------- oops!
      ( else
           (error 'preprocess "invalid expression ~a" exp) ))))

;; -------------------------------------------------------------------------
;; These functions work on expressions using the core language.
;; They deal with bound and free variables.
;; -------------------------------------------------------------------------

(define occurs-bound?
  (lambda (s exp)
    (cond ((varref? exp) #f)
          ((app? exp)    (or (occurs-bound? s (app->proc exp))
                             (occurs-bound? s (app->arg  exp))))
          ((lambda? exp) (or (occurs-bound? s (lambda->body exp))
                             (and (eq? s (lambda->param exp))
                                  (occurs-free? s (lambda->body exp)))))
          (else (error 'occurs-bound? "invalid expression ~a" exp)))))

(define occurs-free?
  (lambda (s exp)
    (cond ((varref? exp) (eq? s exp))
          ((app? exp)    (or (occurs-free? s (app->proc exp))
                             (occurs-free? s (app->arg  exp))))
          ((lambda? exp) (and (not (eq? s (lambda->param exp)))
                              (occurs-free? s (lambda->body exp))))
          (else (error 'occurs-free? "invalid expression ~a" exp)))))

;; -------------------------------------------------------------------------

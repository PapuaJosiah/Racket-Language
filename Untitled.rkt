#lang racket
(require "syntax-procs.rkt")
(require "free-vars.rkt")
(require "list-indes.rkt")
(provide lexical-address)

(define lexical-address
  (lambda (exp)
    (lexical-address-helper exp (list (free-vars exp)))))

(define lexical-address-helper
  (lambda (exp list-of-decls)
    (cond ((varref? exp)
           (lexical-address-for exp list-of-decls 0))
           ((app? exp)
            (make-app (lexical-address-helper (app->proc exp) list-of-decls)
                      (map (lambda (e) (lexical-address-helper e list-of-decls)) (app->args exp))))
           ((if? exp)
            (make-if (lexical-address-helper (if->test exp) list-of-decls)
                     (lexical-address-helper (if->then exp) list-of-decls)
                     (lexical-address-helper (if->else exp) list-of-decls)))
           ((lambda? exp)
            (make-lambda
             (lambda->params exp)
             (lexical-address-helper (lambda->body exp)
                                     (cons (lambda->params exp) list-of-decls))))
           (else (error 'lexical-address-helper
                        "illegal expression ~a" exp)))))

(define lexical-address-for
  (lambda (varref list-of-decls depth)
    (if (null? list-of-decls)
        (error 'lexical-address-for "unknown varref ~a" varref)
        (if (member varref (first list-of-decls))
            (list varref ': depth
                  (list-index varref (first list-of-decls)))
            (lexical-address-for varref (rest list-of-decls) (add1 depth))))))
    
            
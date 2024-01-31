#lang racket

(require "syntax-procs.rkt")
(require "occurs-procs.rkt")
(provide tree-count annotate annotate-ne slist-contains? symexp-contains? contains-varref?)

;; ----------------------- Problem 1 ----------------------- ;;

(define tree-count
  (lambda (test? tree)
    (if (number? tree)
        (if (test? tree)
            1
            0)
        (if (null? tree)
            0
            (+ (tree-count test? (second tree))
               (tree-count test? (third tree))
               (tree-count test? (first tree)))))))

(tree-count positive? '(1 (2 3 4) 5))
(tree-count (lambda (n) (> n 10))
            '(8 (13 11 (5 24 6)) (15 (12 10 14) 20)))
""
;; ----------------------- Problem 2 ----------------------- ;;

(define annotate
  (lambda (exp)
    (annotate-ne exp 0))) ; interface procedure

(define annotate-ne
  (lambda (exp level)
    (if (number? exp)
        exp
        (cons ;pair (x . x)
         (cons (first exp) level) ; first_item = pair ((x . x) . x)
         (list (annotate-ne (second exp) (+ level 1)) ; second_item = list ((x . x) . '())
               (annotate-ne (third exp) (+ level 1)))))))

(annotate '(+ 4 5))
(annotate '(* (+ 4 5)
                     (/ 7 6)))
(annotate '(* (+ 4 5)
                     (/ 7
                        (+ 6 5))))
""
;; ----------------------- Problem 3 ----------------------- ;;

(define slist-contains?
  (lambda (s slst)
    (if (null? slst)
        #f
        (if (symexp-contains? s (car slst))
            #t
            (slist-contains? s (cdr slst))))))

(define symexp-contains?
  (lambda (sym exp)
    (if (symbol? exp)
        (eq? sym exp)
        (if (null? exp)
            #f
            (or (symexp-contains? sym (car exp))
                (symexp-contains? sym (cdr exp)))))))
  
(slist-contains? 'eugene '((a b) (((b g r) (f r)) c (d e)) b))
(slist-contains? '/ '(if (zero? n) zero (/ total n)))
""
;; ----------------------- Problem 4 ----------------------- ;;

;How does program derivation improve the efficiency of a mutually-recursive solution?
;
;
;
;This version of the factorial function is tail recursive:
;     (define factorial
;       (lambda (n acc)
;         (if (zero? n)
;             acc
;             (factorial (sub1 n) (* n acc)))))
;What does that statement mean, in terms of the way the function executes at run time?
;
;
;
;A number can be defined inductively as a sequence of digits:
;     <digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
;
;    <number> ::= <digit>
;               | (10 * <number>) + <digit>
;Write the framework of the if (or cond) expression
;that will be the main control structure of any
;structurally-recursive function that processes numbers of this form.
;Use comments for the 'then' and 'else' clauses to show the cases they handle.
;
;
;
;The expression
;          (occurs-free? x exp)
;is not always equal to the expression
;          (not (occurs-bound? x exp))
;for a given x and exp. 
;
;Give an example that illustrates why.
;
;There are two ways in which this is true. Give distinct examples that illustrate both ways.

;; ----------------------- Problem 5 ----------------------- ;;

(define contains-varref?
  (lambda (v exp)
    (if (varref? exp)
        (eq? v exp)
        (if (lambda? exp)
            (and (not (eq? v (lambda->param exp)))
                 (contains-varref? v (lambda->body exp)))
            (if (app? exp)
                (or (contains-varref? v (app->proc exp))
                    (contains-varref? v (app->arg exp)))
                #f)))))

(contains-varref? 'y 'y) 
(contains-varref? 'x '(lambda (x) x)) 
(contains-varref? 'y '(lambda (y) x)) 
(contains-varref? 'x '(f (lambda (y) x)))
(contains-varref? 'x '( (lambda (x) y) 
                        (lambda (y) x) ))

;; --------------------------------------------------------- ;;
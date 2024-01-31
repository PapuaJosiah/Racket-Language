;;  ------------------------------------------------------------------------
;; |                                                                        |
;; |   FILE           :  syntax-procs.rkt                                   |
;; |   AUTHOR         :  Eugene Wallingford                                 |
;; |                                                                        |
;; |   DESCRIPTION    :  Several generic helper procedures that extend      |
;; |                     Racket's functionality.                            |
;; |                                                                        |
;;  ------------------------------------------------------------------------

#lang racket

(provide displayln
         list-of?
         list-index)

;; --------------------------------------------------------------------------
;; I/O
;; --------------------------------------------------------------------------

(define displayln
  (lambda lst
    (begin
      (for-each display lst)
      (newline))))

;; --------------------------------------------------------------------------
;; lists
;; --------------------------------------------------------------------------

(define list-of?
  (lambda (n)
    (lambda (exp)
      (and (list? exp)
           (= n (length exp))))))

;; --------------------------------------------------------------------------

(define list-index
  (lambda (target los)
    (list-index-helper target los 0)))

(define list-index-helper
  (lambda (target los base)
    (if (null? los)
        -1
        (if (eq? target (car los))
            base
            (list-index-helper target (cdr los) (+ base 1))))))

;; --------------------------------------------------------------------------


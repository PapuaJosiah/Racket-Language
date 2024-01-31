#lang racket
(require rackunit)
(provide lookup)

(define lookup
  (lambda (sym lop)
    (if (null? lop)
        (error 'lookup "invalid variable reference ~a" sym)
        (if (eq? sym (car (first lop)))
            (cdr (first lop))
            (lookup sym (rest lop))))))

(check-equal? (lookup 'e '((e . "Eugene") (w . "Wallingford"))) "Eugene")
(check-equal? (lookup 'w '((e . "Eugene") (w . "Wallingford"))) "Wallingford")
(check-exn    exn:fail?
  (lambda () (lookup 'not-there '((e . "Eugene") (w . "Wallingford")))))

; end of file
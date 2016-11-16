(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.
(define (map f x)
    (if (eq? x nil) nil
      (cons (f (car x)) (map f (cdr x)))))

(define (cons-all elem seq)
  (map (lambda (x) (cons elem x)) seq)
  )

(define (zip pairs)
  (if (eq? (car pairs) nil) nil
    (cons (map car pairs) (zip (map cdr pairs)))
  )
)

(define (size l)
  (if (null? l) 0
    (+ 1 (size (cdr l)))
  )
)

(define (range n)
  (if (eq? n 0) (list 0)
    (append (range (- n 1)) (list n))
  )
)

;; Problem 17
;; Returns a list of two-element lists
(define (enumerate s)
  (if (null? s) s
      (zip (list (range (- (size s) 1)) s))
  )
)


;; Problem 18
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  (cond
    ((= total 0) (cons nil nil))
    ((or (< total 0) (eq? denoms nil)) nil)
    (else (append (cons-all (car denoms) (list-change (- total (car denoms)) denoms)) (list-change total (cdr denoms))))
  )
)
  ; END PROBLEM 18

;; Problem 19
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
         expr
         )
        ((quoted? expr)
          expr
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
            (cons 'lambda (cons params (let-to-lambda body)))
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
            (cons (cons 'lambda (cons (car (zip values)) (let-to-lambda body))) (let-to-lambda (cadr (zip values))))
           ))
        (else
          (map let-to-lambda expr)
         )))

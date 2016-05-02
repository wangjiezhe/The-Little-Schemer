;;; 10. What Is the Value of All of This?

(load "7-relations.scm")

(define new-entry
  build)

(define (lookup-in-entry name entry entry-f)
  (lookup-in-entry-help name
                        (first entry)
                        (second entry)
                        entry-f))

(define (lookup-in-entry-help name names values entry-f)
  (cond
   [(null? names) (entry-f name)]
   [(eq? (car names) name)
    (car values)]
   [else (lookup-in-entry-help name
                               (cdr names)
                               (cdr values)
                               entry-f)]))

(define extend-table
  cons)

(define (lookup-in-table name table table-f)
  (cond
   [(null? table) (table-f name)]
   [else (lookup-in-table name
                          (cdr table)
                          (lambda (name)
                            (lookup-in-table name
                                             (cdr table)
                                             table-f)))]))

(define (expression-to-action e)
  (cond
   [(atom? e) (atom-to-action e)]
   [else (list-to-action e)]))

(define (atom-to-action e)
  (cond
   [(number? e) *const]
   [(eq? e #t) *const]
   [(eq? e #f) *const]
   [(eq? e 'cons) *const]
   [(eq? e 'car) *const]
   [(eq? e 'cdr) *const]
   [(eq? e 'null?) *const]
   [(eq? e 'eq?) *const]
   [(eq? e 'atom?) *const]
   [(eq? e 'zero?) *const]
   [(eq? e 'add1) *const]
   [(eq? e 'sub1) *const]
   [(eq? e 'number?) *const]
   [else *identifier]))

(define (list-to-action e)
  (cond
   [(atom? (car e))
    (cond
     [(eq? (car e) 'quote) *quote]
     [(eq? (car e) 'lambda) *lambda]
     [(eq? (car e) 'cond) *cond]
     [else *application])]
   [else *application]))

(define (value e)
  (meaning e '()))

(define (meaning e table)
  ((expression-to-action e) e table))

(define (*const e table)
  (cond
   [(number? e) e]
   [(eq? e #t) #t]
   [(eq? e #f) #f]
   [else (build 'primitive e)]))

(define (*quote e table)
  (text-of e))

(define text-of
  second)

(define (*identifier e table)
  (lookup-in-table e table initial-table))

(define (initial-table name)
  (car '()))

(define (*lambda e table)
  (build 'non-primitive
         (cons table (cdr e))))

(define table-of
  first)

(define formals-of
  second)

(define body-of
  third)

(define (evcon lines table)
  (cond
   [(else? (question-of (car lines)))
    (meaning (answer-of (car lines) table))]
   [(meaning (question-of (car lines)) table)
    (meaning (answer-of (car lines)) table)]
   [else (evcon (cdr lines) table)]))

(define (else? x)
  (cond
   [(atom? x) (eq? x 'else)]
   [else #f]))

(define question-of
  first)

(define answer-of
  second)

(define (*cond e table)
  (evcon (cond-lines-of e) table))

(define cond-lines-of
  cdr)

(define (evlis arg table)
  (cond
   [(null? args) '()]
   [else (cons (meaning (car args) table)
               (evlis (cdr args) table))]))

(define (*application e table)
  (apply (meaning (function-of e) table)
         (evlis (arguments-of e) table)))

(define function-of
  car)

(define arguments-of
  cdr)

(define (primitive? l)
  (eq? (first l) 'primitive))

(define (non-primitive? l)
  (eq? (first l) 'non-primitive))

(define (apply fun vals)
  (cond
   [(primitive? fun)
    (apply-primitive (second fun) vals)]
   [(non-primitive? fun)
    (apply-closure (second fun) vals)]))

(define (apply-primitive name vals)
  (cond
   [(eq? name 'cons)
    (cons (first vals) (second vals))]
   [(eq? name 'car)
    (car (first vals))]
   [(eq? name 'cdr)
    (cdr (first vals))]
   [(eq? name 'null?)
    (null? (first vals))]
   [(eq? name 'eq?)
    (eq? (first vals) (second vals))]
   [(eq? name 'atom?)
    (:atom? (first vals))]
   [(eq? name 'zero?)
    (zero? (first vals))]
   [(eq? name 'add1)
    (add1 (first vals))]
   [(eq? name 'sub1)
    (sub1 (first vals))]
   [(eq? name 'number?)
    (number? (first vals))]))

(define (:atom? x)
  (cond
   [(atom? x) #t]
   [(null? x) #f]
   [(eq? (car x) 'primitive) #t]
   [(eq? (car x) 'non-primitive) #t]
   [else #f]))

(define (apply-closure closure vals)
  (meaning (body-of closure)
           (extend-table (new-entry (formals-of closure)
                                    vals)
                         (table-of closure))))

;;; 7. Friends and Relations

(load "0-preface.scm")
(load "2-do-it.scm")
(load "3-cons.scm")

(define (set? lat)
  (cond
   [(null? lat) #t]
   [(member? (car lat) (cdr lat)) #f]
   [else (set? (cdr lat))]))

(define (makeset lat)
  (cond
   [(null? lat) '()]
   [(member? (car lat) (cdr lat))
    (makeset (cdr lat))]
   [else (cons (car lat)
               (makeset (cdr lat)))]))

(define (makeset lat)
  (cond
   [(null? lat) '()]
   [else (makeset (multirember (car lat) (cdr lat)))]))

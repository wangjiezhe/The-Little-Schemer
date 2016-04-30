;;; 2. Do It, Do It Adain, and Again, and Again...

(load "0-preface.scm")

;;; If l as a list of atom.
(define (lat? l)
  (cond
   [(null? l) #t]
   [(atom? (car l)) (lat? (cdr l))]
   [else #f]))


;;; If a is a member of lat
(define (member? a lat)
  (cond
   [(null? lat) #f]
   [else (or (eq? (car lat) a)
             (member? a (cdr lat)))]))



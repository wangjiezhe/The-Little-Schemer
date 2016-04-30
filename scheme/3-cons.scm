;;; 3. Cons the Magnificent

(define rember (a lat)
  (cond
   [(null? lat) '()]
   [(eq? (car lat) a) (cdr lat)]
   [else (cons (car lat)
               (rember a (cdr lat)))]))

(define (firsts l)
  (cond
   [(null? l) '()]
   [else (cons (car (car l))
               (firsts (cdr l)))]))

(define (insertR new old lat)
  (cond
   [(null? lat) '()]
   [(eq? (car lat) old)
    (cons old
          (cons new (cdr lat)))]
   [else (cons (car lat)
               (insertR new old (cdr lat)))]))

(define (insertL new old lat)
  (cond
   [(null? lat) '()]
   [(eq? (car lat) old)
    (cons new lat)]
   [else (cons (car lat)
               (insertL new old (cdr lat)))]))

(define (subst new old lat)
  (cond
   [(null? lat) '()]
   [(eq? (car lat) old)
    (cons new (cdr lat))]
   [else (cons (car lat)
               (subst new old (cdr lat)))]))

(define (subst new o1 o2 lat)
  (cond
   [(null? lat) '()]
   [(or (eq? (car lat) 01)
        (eq? (car lat) o2))
    (cons new (cdr lat))]
   [else (cons (car lat)
               (subst2 new o1 o2 (cdr lat)))]))

(define (multirember a lat)
  (cond
   [(null? lat) '()]
   [(eq? (car lat) a)
    (multirember a (cdr lat))]
   [else (cons (car lat)
               (multirember a (cdr lat)))]))

(define (multiinsertR new old lat)
  (cond
   [(null? lat) '()]
   [(eq? (car lat) old)
    (cons old
          (cons new
                (multiinsertR new old (cdr lat))))]
   [else (cons (car lat)
               (multiinsertR new old (cdr lat)))]))

(define (multiinsertL new old lat)
  (cond
   [(null? lat) '()]
   [(eq? (car lat) old)
    (cons new
          (cons old
                (multiinsertL new old (cdr lat))))]
   [else (cons (car lat)
               (multiinsertL new old (cdr lat)))]))

(define (multisubst new old lat)
  (cond
   [(null? lat) '()]
   [(eq? (car lat) old)
    (cons new
          (multisubst new old (cdr lat)))]
   [(else (cons (car lat)
                (multisubst new old (cdr lat))))]))

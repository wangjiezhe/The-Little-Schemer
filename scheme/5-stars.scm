;;; 5. *Oh My Gawd*: It's FUll of Stars

(load "0-preface.scm")
(load "4-numbers.scm")

(define (rember* a l)
  (cond
   [(null? l) '()]
   [(atom? (car l))
    (cond
     [(eq? (car l) a)
      (rember* a (cdr l))]
     [else (cons (car l)
                 (rember* a (cdr l)))])]
   [else (cons (rember* a (car l))
               (rember* a (cdr l)))]))

(define (insertR* new old l)
  (cond
   [(null? l) '()]
   [(atom? (car l))
    (cond
     [(eq? (car l) old)
      (cons old
            (cons new
                  (insertR* new old (cdr l))))]
     [else (cons old
                 (insertR* new old (cdr l)))])]
   [else (cons (insertR* new old (car l))
               (insertR* new old (cdr l)))]))

(define (occur* a l)
  (cond
   [(null? l) 0]
   [(atom? (car l))
    (cond
     [(eq? (car l) a)
      (add1 (occur* a (cdr l)))]
     [else (occur* a (cdr l))])]
   [else (o+ (occur* a (car l))
             (occur* a (cdr l)))]))

(define (subst* new old l)
  (cond
   [(null? l) '()]
   [(atom? (car l))
    (cond
     [(eq? (car l) old)
      (cons new
            (subst* new old (cdr l)))])]
   [else (cons (subst* new old (car l))
               (subst* new old (cdr l)))]))

(define (insertL* new old l)
  (cond
   [(null? l) '()]
   [(atom? (car l))
    (cond
     [(eq? (car l) old)
      (cons new
            (cons old
                  (insertL* new old (cdr l))))]
     [else (cons old
                 (insertL* new old (cdr l)))])]
   [else (cons (insertL* new old (car l))
               (insertl* new old (cdr l)))]))

(define (member* a l)
  (cond
   [(null? l) #f]
   [(atom? (car l))
    (or (eq? (car l) a)
        (member* a (cdr l)))]
   [else (or (member* a (car l))
             (member* a (cdr l)))]))

(define (leftmost l)
  (cond
   [(atom? (car l))
    (car l)]
   [else (leftmost (car l))]))

(define (eqlsit? l1 l2)
  (cond
   [(and (null? l1) (null? l2)) #t]
   [(or (null? l1) (null? l2)) #f]
   [(and (atom? (car l1))
         (atom? (car l2)))
    (and (eqan? (car l1) (car l2))
         (eqlist? (cdr l1) (cdr l2)))]
   [(or (atom? (car l1))
        (atom? (car l2)))
    #f]
   [else (and (eqlist? (car l1) (car l2))
              (eqlist? (cdr l1) (cdr l2)))]))

(define (equal? s1 s2)
  (cond
   [(and (atom? s1) (atom? s2))
    (eqan? s1 s2)]
   [(or (atom? s1) (atom? s2))
    #f]
   [else (and (eqlist? (car s1) (car s2))
              (eqlist? (cdr s1) (cdr s2)))]))

(define (rember s l)
  (cond
   [(null? l) '()]
   [(equal? (car l) s) (cdr l)]
   [else (cons (car l)
               (rember s (cdr l)))]))

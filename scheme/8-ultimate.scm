;;; 8. Lambda the Ultimate

(load "0-preface.scm")
(load "4-number.scm")

(define (rember-f test? a l)
  (cond
   [(null? l) '()]
   [(test? (car l) a) (cdr l)]
   [else (cons (car l)
               (rember-f test? a (cdr l)))]))

(define (eq?-c a)
  (lambda (x)
    (eq? x a)))

(define eq?-salad
  (eq?-c 'salad))

(define (rember-f test?)
  (lambda (a l)
    (cond
     [(null? l) '()]
     [(test? (car l) a) (cdr l)]
     [else cons (car l)
           ((rember-f test?) a (cdr l))])))

(define (insertL-f test?)
  (lambda (new old l)
    (cond
     [(null? l) '()]
     [(test? (car l) old)
      (cons new (cons old (cdr l)))]
     [else (cons (car l)
                 ((insertL-f test?) new old (cdr l)))])))

(define (insertR-f test?)
  (lambda (new old l)
    (cond
     [(null? l) '()]
     [(test? (car l) old)
      (cons old (cons new (cdr l)))]
     [else (cons (car l)
                 ((insertR-f test?) new old (cdr l)))])))

(define (seqL new old l)
  (cons new (cons old l)))

(define (seqR new old l)
  (cons old (cons new l)))

(define (insert-g seq)
  (lambda (new old l)
    (cond
     [(null？ l) '()]
     [(eq? (car l) old)
      (seq new old (cdr l))]
     [else (cons (car l)
                 ((insert-g seq) new old (cdr l)))])))

(define insertL
  (insert-g seqL))

(define insertR
  (insert-g seqR))

(define insertL
  (insert-g
   (lambda (new old l)
     (cons new (cons old l)))))

(define (seqS new old l)
  (cons new l))

(define subst
  (insert-g seqS))

(define (seqrem new old l)
  l)

(define (rember a l)
  ((insert-g seqrem) #f a l))

(define (multirember-f test?)
  (lambda (a lat)
    (cond
     [(null? lat) '()]
     [(test? a (car lat))
      ((multirember-f test?) a (cdr lat))]
     [else (cons (car lat)
                 ((multirember-f test?) a (cdr lat)))])))

(define multirember-eq?
  (multirember-f eq?))

(define eq?-tuna
  (eq?-c 'tuna))

(define (multiremberT test? lat)
  (cond
   [(null? lat) '()]
   [(test? (car lat))
    (multiremberT test? (cdr lat))]
   [else (cons (car lat)
               (multiremberT test? (cdr lat)))]))


;;; tail recursion, continuation and CPS

(define (multirember&co a lat col)
  (cond
   [(null? lat)
    (col '() '())]
   [(eq? (car lat) a)
    (multirember&co a (cdr lat)
                    (lambda (newlat seen)
                      (col newlat
                           (cons (car lat) seen))))]
   [else (multirember&co a (cdr lat)
                         (lambda (newlat seen)
                           (col (cons (car lat) newlat)
                                seen)))]))

(define (a-friend x y)
  (null? y))

(define (last-friend x y)
  (length x))

(define (multiinsertLR new oldL oldR lat)
  (cond
   [(null? lat) '()]
   [(eq? (car lat) oldL)
    (cons new
          (cons oldL
                (multiinsertLR new oldL oldR (cdr lat))))]
   [(eq? (car lat) oldR)
    (cons oldR
          (cons new
                (multiinsertLR new oldL oldR (cdr lat))))]
   [else (cons (car lat)
               (multiinsertLR new oldL oldR (cdr lat)))]))

(define (multiinsertLR&co new oldL oldR lat col)
  (cond
   [(null? lat)
    (col '() 0 0)]
   [(eq? (car lat) oldL)
    (multiinsertLR&co new oldL oldR (cdr lat)
                      (lambda (newlat L R)
                        (col (cons new
                                   (cons oldL newlat))
                             (add1 L) R)))]
   [(eq? (car lat) oldR)
    (multiinsertLR&co new oldL oldR (cdr lat)
                      (lambda (newlat L R)
                        (col (cons oldR
                                   (cons new newlat))
                             L (add1 R))))]
   [else (multiinsertLR&co new oldL oldR (cdr lat)
                           (lambda (newlat L R)
                             (col (cons (car lat) newlat)
                                  L R)))]))

(define (display-all . vs)
  (for-each (lambda (x)
              (display x)
              (newline))
            vs))

(define (evens-only* l)
  (cond
   [(null? l) '()]
   [(atom? (car l))
    (cond
     [(even? (car l))
      (cons (car l) (evens-only* (cdr l)))]
     [else (evens-only* (cdr l))])]
   [else (cons (evens-only* (car l))
               (evens-only* (cdr l)))]))

(define (evens-only*&co l col)
  (cond
   [(null? l)
    (col '() 1 0)]
   [(atom? (car l))
    (cond
     [(even? (car l))
      (evens-only*&co (cdr l)
                      (lambda (newl p s)
                        (col (cons (car l) newl)
                             (o* (car l) p) s)))]
     [else (evens-only*&co (cdr l)
                           (lambda (newl p s)
                             (col newl
                                  p (o+ (car l) s))))])]
   [else (evens-only*&co (car l)
                         (lambda (al ap as)
                           (evens-only*&co (cdr l)
                                           (lambda (dl dp ds)
                                             (col (cons al dl)
                                                  (o* ap dp)
                                                  (o+ as ds))))))]))
(define (the-last-friend newl product sum)
  (cons sum
        (cons product
              newl)))

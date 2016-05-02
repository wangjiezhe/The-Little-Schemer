;;; 9. ...and Again, and Again, and Again,...

(load "0-preface.scm")
(load "4-number.scm")
(load "7-relations.scm")

(define (looking a lat)
  (keep-looking a (pick 1 lat) lat))

(define (keep-looking a sorn lat)
  (cond
   [(number? sorn)
    (keep-looking a (pick sorn lat) lat)]
   [else (eq? sorn a)]))

(define (eternity x)
  (eternity x))

(define (shift pair)
  (build (first (first pair))
         (build (second (first pair))
                (second pair))))

(define (align pora)
  (cond
   [(atom? pora) pora]
   [(a-pair? (first pora))
    (align (shift pora))]
   [else (build (first pora)
                (align (second pora)))]))

(define (length* pora)
  (cond
   [(atom? pora) 1]
   [else (+ (length* (first pora))
            (length* (second pora)))]))

(define (weight* pora)
  (cond
   [(atom? pora) 1]
   [else (+ (* (weight* (first pora)) 2)
            (weight* (second pora)))]))

(define (shuffle pora)
  (cond
   [(atom? pora) pora]
   [(a-pair? (first pora))
    (shuffle (revpair pora))]
   [else (build (first pora)
                (shuffle (second pora)))]))

;;; Collatz conjecture
(define (C n)
  (cond
   [(one? n) 1]
   [(even? n)
    (C (o/ n 2))]
   [else (C (add1 (o* 3 n)))]))

;;; Ackermann function
(define (A n m)
  (cond
   [(zero? n) (add1 m)]
   [(zero? m) (A (sub1 n) 1)]
   [else (A (sub1 n)
            (A n (sub1 m)))]))

;; (define (will-stop? f)
;;   ...)

(define (last-try x)
  (and (will-stop? last-try)
       (eternity x)))

(define length
  (lambda (l)
    (cond
     [(null? l) 0]
     [else (add1 (length (cdr l)))])))

;;; length_0
(lambda (l)
  (cond
   [(null? l) 0]
   [else (add1 (eternity (cdr l)))]))

;;; length_<=1
(lambda (l)
  (cond
   [(null? l) 0]
   [else (add1 ((lambda (l)
                  (cond
                   [(null? l) 0]
                   [else (add1 (eternity (cdr l)))]))
                (cdr l)))]))

;;; length_<=2
(lambda (l)
  (cond
   [(null? l) 0]
   [else (add1 ((lambda (l)
                  (cond
                   [(null? l) 0]
                   [else (add1 ((lambda (l)
                                  (cond
                                   [(null? l) 0]
                                   [else (add1 (eternity (cdr l)))]))
                                (cdr l)))]))
                (cdr l)))]))

;;; length_0
((lambda (length)
   (lambda (l)
     (cond
      [(null? l) 0]
      [else (add1 (length (cdr l)))])))
 eternity)

;;; length_<=1
((lambda (f)
   (lambda (l)
     (cond
      [(null? l) 0]
      [else (add1 (f (cdr l)))])))
 ((lambda (g)
    (lambda (l)
      (cond
       [(null? l) 0]
       [else (add1 (g (cdr l)))])))
  eternity))

;;; length_<=2
((lambda (length)
   (lambda (l)
     (cond
      [(null? l) 0]
      [else (add1 (length (cdr l)))])))
 ((lambda (length)
    (lambda (l)
      (cond
       [(null? l) 0]
       [else (add1 (length (cdr l)))])))
  ((lambda (length)
     (lambda (l)
       (cond
        [(null? l) 0]
        [else (add1 (length (cdr l)))])))
   eternity)))

;;; length_0
((lambda (mk-length)
   (mklength eternity))
 (lambda (length)
   (lambda (l)
     (cond
      [(null? l) 0]
      [else (add1 (length (cdr l)))]))))

;;; length_<=1
((lambda (mk-length)
   (mk-length
    (mk-length eternity)))
 (lambda (length)
   (lambda (l)
     (cond
      [(null? l) 0]
      [else (add1 (length (cdr l)))]))))

;;; length_<=2
((lambda (mk-length)
   (mk-length
    (mk-length
     (mk-length eternity))))
 (lambda (length)
   (lambda (l)
     (cond
      [(null? l) 0]
      [else (add1 (length (cdr l)))]))))

;;; length_<=3
((lambda (mk-length)
   (mk-length
    (mk-length
     (mk-length
      (mk-length eternity)))))
 (lambda (length)
   (lambda (l)
     (cond
      [(null? l) 0]
      [else (add1 (length (cdr l)))]))))

;;; length_0
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (length)
   (lambda (l)
     (cond
      [(null? l) 0]
      [else (add1 (length (cdr l)))]))))

;;; length_0
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
      [(null? l) 0]
      [else (add1 (mk-length (cdr l)))]))))

;;; length_<=1
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
      [(null? l) 0]
      [else (add1 ((mk-length eternity) (cdr l)))]))))

;;; length
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
      [(null? l) 0]
      [else (add1 ((mk-length mk-length) (cdr l)))]))))

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   ((lambda (length)
       (lambda (l)
         (cond
          [(null? l) 0]
          [else (add1 (length (cdr l)))])))
    (mk-length mk-length))))

(lambda (x)
  ((mk-length mk-length) x))

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
      [(null? l) 0]
      [else (add1 ((lambda (x)
                     ((mk-length mk-length) x))
                   (cdr l)))]))))

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   ((lambda (length)
      (lambda (l)
        (cond
         [(null? l) 0]
         [else (add1 (length (cdr l)))])))
    (lambda (x)
      ((mk-length mk-length) x)))))

((lambda (le)
   ((lambda (mk-length)
      (mk-length mk-length))
    (lambda (mk-length)
      (le (lambda (x)
            ((mk-length mk-length) x))))))
 (lambda (length)
      (lambda (l)
        (cond
         [(null? l) 0]
         [else (add1 (length (cdr l)))]))))

(lambda (le)
  ((lambda (mk-length)
     (mk-length mk-length))
   (lambda (mk-length)
     (le (lambda (x)
           ((mk-length mk-length) x))))))

;;; applicative-order Y combinator
(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))

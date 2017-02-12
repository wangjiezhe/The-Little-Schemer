#lang tls

; help data for debug
(define a "hotdogs")
(define b 4)
(define c 5)
(define d 6)
(define l `((,a ,b) ,c ,d))



;;; Preface

; If x is an atom
#;(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))



;;; 1. Toys

; null? pair? eq? number? zero?
; car cdr cons
; define lambda
; and or
; cond else
; quote
; add1 sub1
; #f #t



;;; 2. Do It, Do It Again, and Again, and Again...

; If l is a list of atoms
(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

; If a is a member of lat
(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? a (car lat))
                (member? a (cdr lat)))))))



;;; 3. Cons the Magnificent

; Remove the first a in lat
(define rember
  (lambda (a lat)
    (cond
      ((null? lat) (quote()))
      ((eq? a (car lat)) (cdr lat))
      (else (cons (car lat)
                  (rember a (cdr lat)))))))

; Collect the first elements of each element of l
(define firsts
  (lambda (l)
    (cond
      ((null? l) (quote()))
      (else (cons (car (car l))
                  (firsts (cdr l)))))))

; Insert new after the first old in lat
(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      (else
       (cond
         ((eq? old (car lat))
          (cons old
                (cons new (cdr lat))))
         (else (cons (car lat)
                     (insertR new old
                              (cdr lat)))))))))

; Insert new before the first old in lat
(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      (else (cond
              ((eq? old (car lat))
               (cons new lat))
              (else (cons (car lat)
                          (insertL new old
                                   (cdr lat)))))))))
; Substitute the first old with new in lat
(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      (else (cond
              ((eq? old (car lat))
               (cons new (cdr lat)))
              (else (cons (car lat)
                          (subst new old (cdr lat)))))))))

; Substitute old with new in lat
(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) (quote()))
      (else (cond
              ((or (eq? o1 (car lat)) (eq? o2 (car lat)))
               (cons new (cdr lat)))
              (else (cons (car lat)
                          (subst2 new o1 o2
                                  (cdr lat)))))))))

; Remove all a in lat
(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) (quote()))
      (else
       (cond
         ((eq? a (car lat))
          (multirember a (cdr lat)))
         (else (cons (car lat)
                     (multirember a (cdr lat)))))))))

; Insert new after all old in lat
(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      (else
       (cond
         ((eq? old (car lat))
          (cons old
                (cons new (multiinsertR new old (cdr lat)))))
         (else (cons (car lat)
                     (multiinsertR new old (cdr lat)))))))))

; Insert new before all old in lat
(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      (else
       (cond
         ((eq? old (car lat))
          (cons new
                (cons old (multiinsertL new old (cdr lat)))))
         (else (cons (car lat)
                     (multiinsertL new old (cdr lat)))))))))

; Substitute all old with new in lat
(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      (else
       (cond
         ((eq? old (car lat))
          (cons new
                (multisubst new old (cdr lat))))
         (else (cons (car lat)
                     (multisubst new old (cdr lat)))))))))



; 4. Numbers Games

; n plus m
(define o+
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (o+ n (sub1 m)))))))

; n minus m
(define o-
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (o- n (sub1 m)))))))

; If l is a tuple of numbers
(define tup?
  (lambda (l)
    (cond
      ((null? l) #t)
      (else (and (number? (car l)) (tup? (cdr l)))))))

; Add up all numbers in tuple
(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (o+ (car tup) (addtup (cdr tup)))))))

; n times m
(define ox
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (o+ n (ox n (sub1 m)))))))

; Plus elements in tup1 and tup2 correspondingly and get a new tuple
(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else
       (cons (o+ (car tup1) (car tup2))
             (tup+ (cdr tup1) (cdr tup2)))))))

; If n is greater than m
(define o>
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (o> (sub1 n) (sub1 m))))))

; If n is less than m
(define o<
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (o< (sub1 n) (sub1 m))))))

; If n is equal to m
(define o=
  (lambda (n m)
    (cond
      ((zero? m) (zero? n))
      ((zero? n) #f)
      (else (o= (sub1 n) (sub1 m))))))

; If n is equal to m
(define oo=
  (lambda (n m)
    (cond
      ((or (o> n m) (o< n m)) #f)
      (else #t))))

; n to the mth
(define o^
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (ox n (o^ n (sub1 m)))))))

; n divided by m
(define o/
  (lambda (n m)
    (cond
      ((o< n m) 0)
      (else (add1 (o/ (o- n m) m))))))

; The length of lat
(define length
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (length (cdr lat)))))))

; Get the nth element in lat
(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

; Remove the nth element in lat
(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else (cons (car lat)
                  (rempick (sub1 n) (cdr lat)))))))

; Remove the number element in lat
(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote()))
      (else (cond
              ((number? (car lat)) (no-nums (cdr lat)))
              (else (cons (car lat)
                          (no-nums (cdr lat)))))))))

; Remove the element that not is a number in lat
(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote()))
      (else (cond
              ((number? (car lat))
               (cons (car lat)
                     (all-nums (cdr lat))))
              (else (all-nums (cdr lat))))))))

; If a1 is equivalent to b
(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2))
       (o= a1 a2))
      ((or (number? a1) (number? a2))
       #f)
      (else (eq? a1 a2)))))

; The times that a occur in lat
(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      (else (cond
              ((eq? a (car lat))
               (add1 (occur a (cdr lat))))
              (else (occur a (cdr lat))))))))

; If n is equal to 1
(define one?
  (lambda (n)
    (cond
      ((zero? n) #f)
      (else (zero? (sub1 n))))))

; If an is equal to 1
(define one2?
  (lambda (n)
    (o= n 1)))

; Remove the nth element in lat
(define rempick2?
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else (cons (car lat)
                  (rempick2? (sub1 n)
                            (cdr lat)))))))



; 5. *Oh My Gawd*: It's Full of Stars

; Remove a in l recursively
(define rember*
  (lambda (a l)
    (cond
      ((null? l) (quote()))
      ((atom? (car l))
       (cond
         ((eq? a (car l))
          (rember* (cdr l)))
         (else (cons (car l)
                     (rember* (cdr l))))))
      (else (cons (rember* (car l))
                  (rember* (cdr l)))))))

; Insert new after old in l recursively
(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) (quote()))
      ((atom? (car l))
       (cond
         ((eq? old (car l))
          (cons old (cons new
                          (insertR* new old (cdr l)))))
         (else (cons (car l)
                     (insertR* new old (cdr l))))))
      (else (cons (insertR* new old (car l))
                  (insertR* new old (cdr l)))))))

; Times that a occur in l recursively
(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
       (cond
         ((eq? a (car l))
          (add1 (occur* (cdr l))))
         (else (occur* (cdr l)))))
      (else
       (o+ (occur* (car l))
           (occur* (cdr l)))))))

; Substitute old with new in l recursively
(define subst*
  (lambda (new old l)
    (cond
      ((null? l) (quote()))
      ((atom? (car l))
       (cond
         ((eq? old (car l))
          (cons new
                (subst* new old (cdr l))))
         (else
          (cons (car l)
                (subst* new old (cdr l))))))
      (else
       (cons (subst* new old (car l))
             (subst* new old (cdr l)))))))

; Insert new befor old in l recursively
(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) (quote()))
      ((atom? (car l))
       (cond
         ((eq? old (car l))
          (cons new
                (cons old
                      (insertL* new old (cdr l)))))
         (else
          (cons (car l)
                (insertL* new old (cdr l))))))
      (else
       (cons (insertL* new old (car l))
             (insertL* new old (cdr l)))))))

; Check if a is in l recursively
(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
       (or (eq? a (car l))
           (member* a (cdr l))))
      (else
       (or (member* a (car l))
           (member* a (cdr l)))))))

; Find the leftmost atom in l
(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))

; Check if two lists, l1 and l2, are equal
(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      ((and (atom? (car l1)) (atom? (car l2)))
       (and (eqan? (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2))))
      ((or (atom? (car l1)) (atom? (car l2))) #f)
      (else
       (and (eqlist? (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2)))))))

; Check if two S-expressions, s1 and s2, are equal
(define equal2?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2))
       (eqan? s1 s2))
      ((or (atom? s1) (atom? s2))
       #f)
      (else (eqlist2? s1 s2)))))

; Check if two lists, l1 and l2, are equal
(define eqlist2?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      (else
       (and (equal2? (car l1) (car l2))
            (eqlist2? (cdr l1) (cdr l2)))))))

; Remove first s in l
(define rember2
  (lambda (s l)
    (cond
      ((null? l) (quote()))
      (else
       (cond
         ((equal2? s (car l))
          (cdr l))
         (else
          (cons (car l)
                (rember  s (cdr l)))))))))

; Remove first s in l
(define rember3
  (lambda (s l)
    (cond
      ((null? l) (quote()))
      ((equal2? s (car l)) (cdr l))
      (else (cons (car l)
                  (rember s (cdr l)))))))



;;; 6. Shadows

; Check if aexp is an arithmetic expression
(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else
       (and (numbered? (car aexp))
            (numbered? (car (cdr (cdr aexp)))))))))

; Check if aexp is an arithmetic expression, including a is a number
(define numbered_cor?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else
       (and (numbered_cor? (car aexp))
            (cond
              ((null? (cdr aexp)) #t)
              (else (numbered_cor? (cdr (cdr aexp))))))))))

; Get the value of expression nexp
(define value1
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (car (cdr nexp)) (quote o+))
       (o+ (value1 (car nexp))
           (value1 (car (cdr (cdr nexp))))))
      ((eq? (car (cdr nexp)) (quote ox))
       (ox (value1 (car nexp))
           (value1 (car (cdr (cdr nexp))))))
      (else
       (o^ (value1 (car nexp))
           (value1 (car (cdr (cdr nexp)))))))))


(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2st-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car aexp)))

; Get the value of the second kind of expression nexp
(define value2
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (operator nexp) (quote o+))
       (o+ (value2 (1st-sub-exp nexp))
           (value2 (2st-sub-exp nexp))))
      ((eq? (operator nexp) (quote ox))
       (ox (value2 (1st-sub-exp nexp))
           (value2 (2st-sub-exp nexp))))
      (else
       (o^ (value2 (1st-sub-exp nexp))
           (value2 (2st-sub-exp nexp)))))))

(define 1st-sub-exp1
  (lambda (aexp)
    (car aexp)))

(define operator1
  (lambda (aexp)
    (car (cdr aexp))))

(define sero?
  (lambda (n)
    (null? n)))

(define edd1
  (lambda (n)
    (cons (quote()) n)))

(define zub1
  (lambda (n)
    (cdr n)))

(define oo+
  (lambda (n m)
    (cond
      ((sero? m) n)
      (else (edd1 (oo+ n (zub1 m)))))))



;;; 7. Friends and Relations

; Check if lat is a set
(define set2?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (set2? (cdr lat))))))

; Change lat to a set
(define makeset
  (lambda (lat)
    (cond
      ((null? lat) (quote()))
      ((member? (car lat) (cdr lat))
       (makeset (cdr lat)))
      (else (cons (car lat)
                  (makeset (cdr lat)))))))

; Change lat to a set
(define makeset2
  (lambda (lat)
    (cond
      ((null? lat) (quote()))
      (else
       (cons
        (car lat)
        (makeset
         (multirember (car lat)
                      (cdr lat))))))))

; Check if set1 is a subset of set2
(define subset2?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else
       (and (member? (car set1) set2)
            (subset2? (cdr set1) set2))))))

; Check two sets, set1 and set2, are equal
(define eqset?
  (lambda (set1 set2)
    (and (subset2? set1 set2)
         (subset2? set2 set1))))

; Check if two sets, set1 and set2, intersect
(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      ((member? (car set1) set2) #t)
      (else (intersect? (cdr set1) set2)))))

; Get the intersection of set1 and set2
(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) (quote()))
      ((member? (car set1) set2)
       (cons (car set1)
             (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2)))))

; Get the union of set1 and set2
(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2)
       (union (cdr set1) set2))
      (else (cons (car set1)
                  (union (cdr set1) set2))))))

; Get all the atoms in set1 that are not in set2
(define diff
  (lambda (set1 set2)
    (cond
      ((null? set1) (quote()))
      ((member? (car set1) set2)
       (diff (cdr set1) set2))
      (else (cons (car set1)
                  (diff (cdr set1) set2))))))

; Get the intersection of all lists in the set l-set of lists
(define intersectall
  (lambda (l-set)
    (cond
      ((null? (cdr l-set)) (car l-set))
      (else (intersect (car l-set)
                       (intersectall (cdr l-set)))))))

; Check if x is a pair
;
; A pair is a list with only two S-expressions
(define a-pair?
  (lambda (x)
    (cond
      ((atom? x) #f)
      ((null? x) #f)
      ((null? (cdr x)) #f)
      ((null? (cdr (cdr x))) #t)
      (else #f))))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 (quote())))))

(define third
  (lambda (l)
    (car (cdr (cdr l)))))

; Check if rel is a function
;
; A finite function is a list of pairs
; in which no first elements of any pair
; is the same as any other first element
(define fun?
  (lambda (rel)
    (set2? (firsts rel))))

; Reverse a pair
(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))

; Reverse pairs in a function rel
(define revrel
  (lambda (rel)
    (cond
      ((null? rel) (quote()))
      (else
       (cons (revpair (car rel))
             (revrel (cdr rel)))))))

; Check if fun is a full function (surjective),
; thus a one-to-one function
(define fullfun?
  (lambda (fun)
    (set2? (seconds fun))))

; Collect the seciond elements of each element of l
(define seconds
  (lambda (l)
    (cond
      ((null? l) (quote()))
      (else
       (cons (car (cdr (car l)))
             (seconds (cdr l)))))))

; Check if fun is a one-to-one function
(define one-to-one?
  (lambda (fun)
    (fun? (revrel fun))))



;;; 8. Lambda the Ultimate

; Remove x's in l such that x->(test? x a) returns true
(define rember-f
  (lambda (test? a l)
    (cond
      ((null? l) (quote()))
      ((test? (car l) a) (cdr l))
      (else (cons (car l)
                  (rember-f test? a (cdr l)))))))

; Return a function x->(eq? x a)
; Curry-ing
(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(define eq?-salad (eq?-c 'salad))

; Return a function (a l)->(rember-f test? a l)
(define rember-f2
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) (quote()))
        ((test? a (car l)) (cdr l))
        (else (cons (car l)
                    ((rember-f2 test?) a (cdr l))))))))

(define rember-eq? (rember-f2 eq?))

; Return a function which insert new before the first x in l
; such that x->(test? x old) returns true
(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) (quote()))
        ((test? (car l) old)
         (cons new l))
        (else (cons (car l)
                    ((insertL-f test?) new old
                                       (cdr l))))))))

; Return a function which insert new after  the first x in l
; such that x->(test? x old) returns true
(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) (quote()))
        ((test? (car l) old)
         (cons old (cons new (cdr l))))
        (else (cons (car l)
                    ((insertR-f test?) new old
                                       (cdr l))))))))

; Help functions for insert-g->insertL-f
(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

; Help function for insert-g->insertL-R
(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

; Use seqL and seqR to construct insertL-f and insertR-f
(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
        ((null? l) (quote()))
        ((eq? (car l) old)
         (seq new old (cdr l)))
        (else (cons (car l)
                    ((insert-g seq) new old
                                    (cdr l))))))))

(define insertL2 (insert-g seqL))

(define insertR2 (insert-g seqR))

(define insertL3
  (insert-g
   (lambda (new old l)
     (cons new (cons old l)))))

(define insertR3
  (insert-g
   (lambda (new old l)
     (cons old (cons new l)))))

; Help function for insert-g->subst
(define seqS
  (lambda (new old l)
    (cons (new l))))

(define subst3 (insert-g seqS))

; Help function for insert-g->rember
(define seqrem
  (lambda (new old l)
    l))

(define rember4
  (lambda (a l)
    ((insert-g seqrem) #f a l)))

; symbol->function
(define atom-to-function
  (lambda (x)
    (cond
      ((eq? x (quote o+)) o+)
      ((eq? x (quote ox)) ox)
      (else o^))))

(define value3
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      (else
       ((atom-to-function (operator nexp))
        (value3 (1st-sub-exp nexp))
        (value3 (2st-sub-exp nexp)))))))

; Return a function which remove all x's in lat
; such that x->(test? x a) returns true
(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) (quote()))
        ((test? (car lat) a)
         ((multirember-f test?) a (cdr lat)))
        (else
         (cons (car lat)
               ((multirember-f test?) a (cdr lat))))))))

(define multirember-eq? (multirember-f eq?))

(define eq?-tuna
  (eq?-c (quote tuna)))

; Remove all x's in lat such that x->(test? x) returns true
(define multiremberT
  (lambda (test? lat)
    (cond
      ((null? lat) (quote()))
      ((test? (car lat))
       (multiremberT test? (cdr lat)))
      (else
       (cons (car lat)
             (multiremberT test? (cdr lat)))))))

; col: collector or continuation
; To collect more than one value at a time
;
; Return (col la lb),
; where la is a list of x's such that x->(test? x a) returns false
; and lb is a list of x's such that x->(test? x b) returns true
(define multirember&co
  (lambda (a lat col)
    (cond
      ((null? lat)
       (col (quote()) (quote())))
      ((eq? (car lat) a)
       (multirember&co a (cdr lat)
                       (lambda (newlat seen)
                         (col newlat
                              (cons (car lat) seen)))))
      (else
       (multirember&co a (cdr lat)
                       (lambda (newlat seen)
                         (col (cons (car lat) newlat)
                              seen)))))))

(define a-friend
  (lambda (x y)
    (null? y)))

(define new-friend
  (lambda (newlat seen)
    (a-friend newlat
              (cons (quote tuna) seen))))

(define latest-friend
  (lambda (newlat seen)
    (a-friend (cons (quote and) newlat)
              seen)))

(define last-friend
  (lambda (x y)
    (length x)))

; Insert new before all oldL's and after all oldR's in lat
(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) oldL)
       (cons new
             (cons oldL
                   (multiinsertLR new oldL oldR
                                  (cdr lat)))))
      ((eq? (car lat) oldR)
       (cons oldR
             (cons new
                   (multiinsertLR new oldL oldR
                                  (cdr lat)))))
      (else
       (cons (car lat)
             (multiinsertLR new oldL oldR
                            (cdr lat)))))))

; Return (col newl L R),
; where newl is the result same with multiinsertLR,
; L is the number of insertions on the left
; and R is the number of insertions on the right
(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
      ((null? lat)
       (col (quote ()) 0 0))
      ((eq? (car lat) oldL)
       (multiinsertLR&co new oldL oldR (cdr lat)
                      (lambda (newlat L R)
                        (col (cons new (cons oldL newlat))
                             (add1 L) R))))
      ((eq? (car lat) oldR)
       (multiinsertLR&co new oldL oldR (cdr lat)
                      (lambda (newlat L R)
                        (col (cons oldR (cons new newlat))
                             L (add1 R)))))
      (else
       (multiinsertLR&co new oldL oldR (cdr lat)
                         (lambda (newlat L R)
                           (col (cons (car lat) newlat)
                                L R)))))))

; If it is a even number
(define even?
  (lambda (n)
    (o= (ox (o/ n 2) 2 ) n)))

; Remove all odd numbers from a list of nested lists
(define evens-only*
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond
         ((even? (car l))
          (cons (car l)
                (evens-only* (cdr l))))
         (else
          (evens-only* (cdr l)))))
      (else
       (cons (evens-only* (car l))
             (evens-only* (cdr l)))))))

; Return (col newl p s),
; where newl is the result same with evens-only*,
; p is the product of the even numbers in l
; and s is the sum of the odd numbers in l
(define evens-only*Eco
  (lambda (l col)
    (cond
      ((null? l)
       (col (quote ()) 1 0))
      ((atom? (car l))
       (cond
         ((even? (car l))
          (evens-only*Eco (cdr l)
                          (lambda (newl p s)
                            (col (cons (car l) newl)
                                 (ox (car l) p) s))))
         (else
          (evens-only*Eco (cdr l)
                          (lambda (newl p s)
                            (col newl
                                 p (o+ (car l) s)))))))
      (else
       (evens-only*Eco (car l)
                        (lambda (al ap as)
                          (evens-only*Eco (cdr l)
                                          (lambda (dl dp ds)
                                            (col (cons al dl)
                                                 (ox ap dp)
                                                 (o+ as ds))))))))))




;;; 9. ...and Again, and Again, and Again,...

; partial function
;
; Try to find a in lat following the number from the first
(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

; sorn: symbol or number
;
; Try to find a in lat beginning with sorn
(define keep-looking
  (lambda (a sorn lat)
    (cond
      ((number? sorn)
       (keep-looking a (pick sorn lat) lat))
      (else
       (eq? sorn a)))))

; most partial function
(define eternity
  (lambda (x)
    (eternity x)))

; ((a b) c)->(a (b c))
(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair))
                  (second pair)))))

; pora: nested a-pair or atom
;
; nested shift
; pora->(a (b (...(d e))))
(define align
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora))
       (align (shift pora)))
      (else
       (build (first pora)
              (align (second pora)))))))

; Count the number of atoms in pora
(define length*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else
       (o+ (length* (first pora))
           (length* (second pora)))))))

; Better function for determining the length of pora,
; with more attention to the first component
(define weight*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else
       (o+ (ox (weight* (first pora)) 2)
           (weight* (second pora)))))))

; partial
(define shuffle
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora))
       (shuffle (revpair pora)))
      (else (build (first pora)
                   (shuffle (second pora)))))))

; partial
;
; Collatz conjecture
(define C
  (lambda (n)
    (cond
      ((one? n) 1)
      ((even? n) (C (o/ n 2)))
      (else (C (add1 (ox 3 n)))))))

; total, not primitive recursive
;
; Ackermann function
(define A
  (lambda (n m)
    (cond
      ((zero? n) (add1 m))
      ((zero? m) (A (sub1 n) 1))
      (else (A (sub1 n)
               (A n (sub1 m)))))))

; stop function
#;(define will-stop?
    (lambda (f)
      ...))

; It turns out that will-stop? cannot be defined
#;(define last-try
    (lambda (x)
      (and (will-stop? last-try)
           (eternity x))))


; length_0
#;(lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (eternity (cdr l))))))

; length_<=1
#;(lambda (l)
    (cond
      ((null? l) 0)
      (else
       (add1
        ((lambda (l)
           (cond
             ((null? l) 0)
             (else (add1
                    (eternity (cdr l))))))
         (cdr l))))))

; length_<=2
#;(lambda (l)
    (cond
      ((null? l) 0)
      (else
       (add1
        ((lambda (l)
           (cond
             ((null? l) 0)
             (else
              (add1
               ((lambda (l)
                  (cond
                    ((null? l) 0)
                    (else
                     (add1
                      (eternity
                       (cdr l))))))
                (cdr l))))))
         (cdr l))))))

; length_0
#;((lambda (length)
     (lambda (l)
       (cond
         ((null? l) 0)
         (else (add1 (length (cdr l)))))))
   eternity)

; length_<=1
#;((lambda (f)
     (lambda (l)
       (cond
         ((null? l) 0)
         (else (add1 (f (cdr l)))))))
   ((lambda (g)
      (lambda (l)
        (cond
          ((null? l) 0)
          (else (add1 (g (cdr l)))))))
    eternity))

; length_<=2
#;((lambda (length)
     (lambda (l)
       (cond
         ((null? l) 0)
         (else (add1 (length (cdr l)))))))
   ((lambda (length)
      (lambda (l)
        (cond
          ((null? l) 0)
          (else (add1 (length (cdr l)))))))
    ((lambda (length)
       (lambda (l)
         (cond
           ((null? l) 0)
           (else (add1 (length (cdr l)))))))
     eternity)))

; mk-length
#;(lambda (length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (length (cdr l)))))))

; length_0
#;((lambda (mk-length)
     (mk-length eternity))
   (lambda (length)
     (lambda (l)
       (cond
         ((null? l) 0)
         (else (add1 (length (cdr l))))))))

; length_<=1
#;((lambda (mk-length)
     (mk-length
      (mk-length eternity)))
   (lambda (length)
     (lambda (l)
       (cond
         ((null? l) 0)
         (else (add1 (length (cdr l))))))))

; length_<=2
#;((lambda (mk-length)
     (mk-length
      (mk-length
       (mk-length eternity))))
   (lambda (length)
     (lambda (l)
       (cond
         ((null? l) 0)
         (else (add1 (length (cdr l))))))))

; length_<=3
#;((lambda (mk-length)
     (mk-length
      (mk-length
       (mk-length
        (mk-length eternity)))))
   (lambda (length)
     (lambda (l)
       (cond
         ((null? l) 0)
         (else (add1 (length (cdr l))))))))

; length_0
#;((lambda (mk-length)
     (mk-length mk-length))
   (lambda (length)
     (lambda (l)
       (cond
         ((null? l) 0)
         (else (add1 (length (cdr l))))))))

; length_0
#;((lambda (mk-length)
     (mk-length mk-length))
   (lambda (mk-length)
     (lambda (l)
       (cond
         ((null? l) 0)
         (else (add1 (mk-length (cdr l))))))))

; length_<=1
#;((lambda (mk-length)
     (mk-length mk-length))
   (lambda (mk-length)
     (lambda (l)
       (cond
         ((null? l) 0)
         (else (add1
                ((mk-length eternity)
                 (cdr l))))))))

; length
#;((lambda (mk-length)
     (mk-length mk-length))
   (lambda (mk-length)
     (lambda (l)
       (cond
         ((null? l) 0)
         (else (add1
                ((mk-length mk-length)
                 (cdr l))))))))

; length
#;((lambda (mk-length)
     (mk-length mk-length))
   (lambda (mk-length)
     ((lambda (length)
        (lambda (l)
          (cond
            ((null? l) 0)
            (else (add1 (length (cdr l)))))))
      (mk-length mk-length))))

; length
#;((lambda (mk-length)
     ((lambda (length)
        (lambda (l)
          (cond
            ((null? l) 0)
            (else (add1 (length (cdr l)))))))
      (mk-length mk-length)))
   (lambda (mk-length)
     ((lambda (length)
        (lambda (l)
          (cond
            ((null? l) 0)
            (else (add1 (length (cdr l)))))))
      (mk-length mk-length))))

; length
#;((lambda (length)
     (lambda (l)
       (cond
         ((null? l) 0)
         (else (add1 (length (cdr l)))))))
   ((lambda (mk-length)
      ((lambda (length)
         (lambda (l)
           (cond
             ((null? l) 0)
             (else (add1 (length (cdr l)))))))
       (mk-length mk-length)))
    (lambda (mk-length)
      ((lambda (length)
         (lambda (l)
           (cond
             ((null? l) 0)
             (else (add1 (length (cdr l)))))))
       (mk-length mk-length)))))

; length
#;((lambda (length)
     (lambda (l)
       (cond
         ((null? l) 0)
         (else (add1 (length (cdr l)))))))
   ((lambda (length)
      (lambda (l)
        (cond
          ((null? l) 0)
          (else (add1 (length (cdr l)))))))
    ((lambda (mk-length)
       ((lambda (length)
          (lambda (l)
            (cond
              ((null? l) 0)
              (else (add1 (length (cdr l)))))))
        (mk-length mk-length)))
     (lambda (mk-length)
       ((lambda (length)
          (lambda (l)
            (cond
              ((null? l) 0)
              (else (add1 (length (cdr l)))))))
        (mk-length mk-length))))))

; length
#;((lambda (mk-length)
     (mk-length mk-length))
   (lambda (mk-length)
     (lambda (l)
       (cond
         ((null? l) 0)
         (else (add1
                ((lambda (x)
                   ((mk-length mk-length) x))
                 (cdr l))))))))

; Surrounded with a lambda for not expanding infinitely
; in applicative order
;
; length
#;((lambda (mk-length)
     (mk-length mk-length))
   (lambda (mk-length)
     ((lambda (length)
        (lambda (l)
          (cond
            ((null? l) 0)
            (else (add1 (length (cdr l)))))))
      (lambda (x)
        ((mk-length mk-length) x)))))

; length
#;((lambda (le)
     ((lambda (mk-length)
        (mk-length mk-length))
      (lambda (mk-length)
        (le (lambda (x)
              ((mk-length mk-length) x))))))
   (lambda (length)
     (lambda (l)
       (cond
         ((null? l) 0)
         (else (add1 (length (cdr l))))))))

; Y
#;(lambda (le)
    ((lambda (mk-length)
       (mk-length mk-length))
     (lambda (mk-length)
       (le (lambda (x)
             ((mk-length mk-length) x))))))

; applicative-order Y combinator
(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))



;;; 10. What Is the Value of All of This?

; entry: a pair of lists whose first list is a set,
;        and the two lists must be of equal length
;
; Build a entry from a set of names and a list of values
(define new-entry build)

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
                          (first entry)
                          (second entry)
                          entry-f)))

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
      ((null? names) (entry-f name))
      ((eq? (car names)) name)
      (else (lookup-in-entry-help name
                                  (cdr names)
                                  (cdr values)
                                  entry-f)))))

; table/environment: a list of entries
(define extend-table cons)

(define lookup-in-table
  (lambda (name table table-f)
    (cond
      ((null? table) (table-f name))
      (else (lookup-in-entry name
                             (car table)
                             (lambda (name)
                               (lookup-in-entry name
                                                (cdr table)
                                                table-f)))))))

(define expression-to-action
  (lambda (e)
    (cond
      ((atom? e) (atom-to-action e))
      (else (list-to-action e)))))

(define atom-to-action
  (lambda (e)
    (cond
      ((number? e) *const)
      ((eq? e #t) *const)
      ((eq? e #f) *const)
      ((eq? e 'cons) *const)
      ((eq? e 'car) *const)
      ((eq? e 'cdr) *const)
      ((eq? e 'null?) *const)
      ((eq? e 'eq?) *const)
      ((eq? e 'atom?) *const)
      ((eq? e 'zero?) *const)
      ((eq? e 'add1) *const)
      ((eq? e 'sub1) *const)
      ((eq? e 'number) *const)
      (else *identifier))))

(define list-to-action
  (lambda (e)
    (cond
      ((atom? (car e))
       (cond
         ((eq? (car e) 'quote) *quote)
         ((eq? (car e) 'lambda) *lambda)
         ((eq? (car e) 'cond) *cond)
         (else *application)))
      (else *application))))

; interpreter
(define value
  (lambda (e)
    (meaning e '())))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

(define *const
  (lambda (e table)
    (cond
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      (else (build 'primitive e)))))

(define *quote
  (lambda (e table)
    (text-of e)))

(define text-of second)

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

(define initial-table
  (lambda (name)
    (car '())))

(define *lambda
  (lambda (e table)
    (build 'non-primitive
           (cons table (cdr e)))))

(define table-of first)
(define formals-of second)
(define body-of third)

(define evcon
  (lambda (lines table)
    (cond
      ((else? (question-of (car lines)))
       (meaning (answer-of (car lines)) table))
      ((meaning (question-of (car lines)) table)
       (meaning (answer-of (car lines) table)))
      (else (evcon (cdr lines) table)))))

(define else?
  (lambda (x)
    (cond
      ((atom? x) (eq? x 'else))
      (else #f))))

(define question-of first)
(define answer-of second)

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

(define cond-lines-of cdr)

(define evlis
  (lambda (args table)
    (cond
      ((null? args) '())
      (else
       (cons (meaning (car args) table)
             (evlis (cdr args) table))))))

(define *application
  (lambda (e table)
    (apply (meaning (function-of e) table)
           (evlis (arguments-of e) table))))

(define function-of car)
(define arguments-of cdr)

(define primitive?
  (lambda (l)
    (eq? (first l) 'primitive)))

(define non-primitive?
  (lambda (l)
    (eq? (first l) 'non-primitive)))

(define apply
  (lambda (fun vals)
    (cond
      ((primitive? fun)
       (apply-primitive (second fun) vals))
      ((non-primitive? fun)
       (apply-closure (second fun) vals)))))

(define apply-primitive
  (lambda (name vals)
    (cond
      ((eq? name 'cons)
       (cons (first vals) (second vals)))
      ((eq? name 'car)
       (car (first vals)))
      ((eq? name 'cdr)
       (cdr (first vals)))
      ((eq? name 'null?)
       (null? (first vals)))
      ((eq? name 'eq?)
       (eq? (first vals) (second vals)))
      ((eq? name 'atom?)
       (:atom? (first vals)))
      ((eq? name 'zero?)
       (zero? (first vals)))
      ((eq? name 'add1)
       (add1 (first vals)))
      ((eq? name 'sub1)
       (sub1 (first vals)))
      ((eq? name 'number?)
       (number? (first vals))))))

(define :atom?
  (lambda (x)
    (cond
      ((atom? x) #t)
      ((null? x) #f)
      ((eq? (car x) 'primitive) #t)
      ((eq? (car x) 'non-primitive) #t)
      (else #f))))

(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
             (extend-table
              (new-entry
               (formals-of closure) vals)
              (table-of closure)))))

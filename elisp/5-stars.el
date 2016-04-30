;;; *Oh My Gawd*: It's Full of Stars

(require '0-preface)
(require '4-numbers)

(defun rember* (a l)
  (cond
   ((null l) (quote ()))
   ((atom? (car l))
    (cond
     ((eq (car l) a)
      (rember* a (cdr l)))
     (t (cons (car l)
              (rember* a (cdr l))))
     ))
   (t (cons (rember* a (car l))
            (rember* a (cdr l))))
   )
  )

(defun insertR* (new old l)
  (cond
   ((null l) (quote ()))
   ((atom? (car l))
    (cond
     ((eq (car l) old)
      (cons old
            (cons new
                  (insertR* new old (cdr l)))))
     (t (cons old (insertR* new old (cdr l))))
     ))
   (t (cons (insertR* new old (car l))
            (insertR* new old (cdr l))))
   )
  )

(defun occur* (a l)
  (cond
   ((null l) 0)
   ((atom? (car l))
    (cond
     ((eq (car l) a)
      (add1 (occur* a (cdr l))))
     (t (occur* a (cdr l)))
     ))
   (t (o+ (occur* a (car l))
          (occur* a (cdr l))))
   )
  )

(defun subst* (new old l)
  (cond
   ((null l) (quote ()))
   ((atom? (car l))
    (cond
     ((eq (car l) old)
      (cons new
            (subst* new old (cdr l))))
     (t (cons (car l)
              (subst* new old (cdr l))))
     ))
   (t (cons (subst* new old (car l))
            (subst* new old (cdr l))))
   )
  )

(defun insertL* (new old l)
  (cond
   ((null l) (quote ()))
   ((atom? (car l))
    (cond
     ((eq (car l) old)
      (cons new
            (cons old
                  (insertL* new old (cdr l)))))
     (t (cons old
              (insertL* new old (cdr l))))
     ))
   (t (cons (insertL* new old (car l))
            (insertL* new old (cdr l))))
   )
  )

(defun member* (a l)
  (cond
   ((null l) nil)
   ((atom? (car l))
    (or (eq (car l) a)
        (member* a (cdr l))))
   (t (or (member* a (car l))
          (member* a (cdr l))))
   )
  )


(defun leftmost (l)
  (cond
   ((atom? (car l)) (car l))
   (t (leftmost (car l)))
   ))

;; (defun eqlist? (l1 l2)
;;   (cond
;;    ((and (null l1) (null l2)) t)
;;    ((and (null l1) (atom? (car l2))) nil)
;;    ((null l1) nil)
;;    ((and (atom? (car l1))
;;          (atom? (car l2)))
;;     (and (eqan? (car l1) (car l2))
;;          (eqlist? (cdr l1) (cdr l2))))
;;    ((atom? (car l1)) nil)
;;    ((null l2) nil)
;;    ((atom? (car l2)) nil)
;;    (t (and (eqlist? (car l1) (car l2))
;;            (eqlist? (cdr l1) (cdr l2))))
;;    ))

(defun eqlist? (l1 l2)
  (cond
   ((and (null l1) (null l2)) t)
   ((or (null l1) (null l2)) nil)
   ((and (atom? (car l1))
         (atom? (car l2)))
    (and (eqan? (car l1) (car l2))
         (eqlist? (cdr l1) (cdr l2))))
   ((or (atom? (car l1))
        (atom? (car l2)))
    nil)
   (t (and (eqlist? (car l1) (car l2))
           (eqlist? (cdr l1) (cdr l2))))
   ))

;; (defun equal? (s1 s2)
;;   (cond
;;    ((and (atom? s1) (atom? s2))
;;     (eqan? s1 s2))
;;    ((atom? s1) nil)
;;    ((atom? s2) nil)
;;    (t (eqlist? s1 s2))
;;    ))

(defun equal? (s1 s2)
  (cond
   ((and (atom? s1) (atom? s2))
    (eqan? s1 s2))
   ((or (atom? s1) (atom? s2))
    nil)
   (t (eqlist? s1 s2))
   ))

(defun rember (s l)
  (cond
   ((null l) (quote ()))
   ((equal? (car l) s) (cdr l))
   (t (cons (car l)
            (rember s (cdr l))))
   ))

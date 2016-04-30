;;; Shadows

(require '0-preface)
(require '4-numbers)

(defun numbered? (aexp)
  (cond
   ((atom? aexp) (number? aexp))
   (t (and (numbered? (car aexp))
           (numbered? (caddr aexp))))
   )
  )

(defun value (nexp)
  (cond
   ((atom? nexp) nexp)
   ((eq (cadr nexp) (quote +))
    (o+ (value (car nexp))
        (value (caddr nexp))))
   ((eq (cadr nexp) (quote *))
    (o* (value (car nexp))
        (value (caddr nexp))))
   (t (o^ (value (car nexp))
          (value (caddr nexp))))
   )
  )

(defun 1st-sub-exp (aexp)
  (car aexp)
  )

(defun 2nd-sub-exp (aexp)
  (caddr aexp)
  )

(defun operator (aexp)
  (cadr aexp)
  )


(defun sero? (n)
  (null n)
  )

(defun edd1 (n)
  (cons (quote ()) n)
  )

(defun zub1 (n)
  (cdr n)
  )

(defun p+ (n m)
  (cond
   ((sero? m) n)
   (t (edd1 (p+ n (zub1 m))))
   )
  )

(provide '6-shadows)

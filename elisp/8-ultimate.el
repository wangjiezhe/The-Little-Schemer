;;; -*- lexical-binding: t -*-
;;; Lambda the Ultimate

(require '0-preface)
(require '4-numbers)
(require '6-shadows)

(defun rember-f (test? a l)
  (cond
   ((null l) (quote ()))
   ((funcall test? (car l) a) (cdr l))
   (t (cons (car l)
            (rember-f test? a (cdr l))))
   )
  )

(defun eq?-c (a)
  (function
   (lambda (x)
     (eq x a)))
  )

(setq eq?-salad (eq?-c 'salad))

(defun rember-f (test?)
  (function
   (lambda (a l)
     (cond
      ((null l) (quote ()))
      ((funcall test? (car l) a) (cdr l))
      (t (cons (car l)
               (funcall (rember-f test?) a (cdr l))))
      )))
  )

(defun insertL-f (test?)
  (function
   (lambda (new old l)
     (cond
      ((null l) (quote ()))
      ((funcall test? (car l) old)
       (cons new (cons old (cdr l))))
      (t (cons (car l)
               (funcall (insertL-f test?) new old (cdr l))))
      )))
  )

(defun insertR-f (test?)
  (function
   (lambda (new old l)
     (cond
      ((null l) (quote ()))
      ((funcall test? (car l) old)
       (cons old (cons new (cdr l))))
      (t (cons (car l)
               (funcall (insertR-f test?) new old (cdr l))))
      )))
  )

(defun seqL (new old l)
  (cons new (cons old l))
  )

(defun seqR (new old l)
  (cons old (cons new l))
  )

(defun insert-g (seq)
  (lambda (new old l)
    (cond
     ((null l) (quote ()))
     ((eq (car l) old)
      (funcall seq new old (cdr l)))
     (t (cons (car l)
              (funcall (insert-g seq) new old (cdr l))))
     ))
  )

;; (setq insertL (insert-g seqL))
;; (setq insertR (insert-g seqR))

(setq insertL
      (insert-g
       (lambda (new old l)
         (cons new (cons old l)))))

(setq insertR
      (insert-g
       (lambda (new old l)
         (cons old (cons new l)))))

(defun seqS (new old l)
  (cons new l)
  )

(setq subst (insert-g #'seqS))

(defun seqrem (new old l)
  l
  )

(defun rember (a l)
  (funcall (insert-g seqrem) nil a l)
  )

(defun atom-to-function (x)
  (cond
   ((eq x (quote +)) #'o+)
   ((eq x (quote *)) #'o*)
   (t #'o^)
   )
  )

(defun value (nexp)
  (cond
   ((atom? nexp) nexp)
   (t (funcall (atom-to-function (operator nexp))
               (value (1st-sub-exp nexp))
               (value (2nd-sub-exp nexp))))
   )
  )

(defun multirember-f (test?)
  (function
   (lambda (a lat)
     (cond
      ((null lat) (quote ()))
      ((funcall test? a (car lat))
       (funcall (multirember-f test?)
                a (cdr lat)))
      )))
  )

(setq multirember-eq?
      (multirember-f #'test?))

(setq eq?-tuna
      (eq?-c (quote tuna)))

(defun multiremberT (test? lat)
  (cond
   ((null lat) (quote ()))
   ((funcall test? (car lat))
    (multiremberT test? (cdr lat)))
   (t (cons (car lat)
            (multiremberT test? (cdr lat))))
   )
  )

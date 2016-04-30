;;; Cons the Magnificent

(require '0-preface)

(defun rember (a lat)
  "Remove a member."
  (cond
   ((null lat) (quote ()))
   ((eq (car lat) a) (cdr lat))
   (t (cons (car lat)
            (rember a (cdr lat))))))

(defun firsts (l)
  (cond
   ((null l) (quote ()))
   (t (cons (car (car l))
            (firsts (cdr l))))))


(defun insertR (new old lat)
  (cond
   ((null lat) (quote ()))
   ((eq (car lat) old)
    (cons old
          (cons new (cdr lat))))
   (t (cons (car lat)
            (insertR new old (cdr lat))))))

(defun insertL (new old lat)
  (cond
   ((null lat) (quote ()))
   ((eq (car lat) old)
    (cons new lat))
   (t (cons (car lat)
            (insertL new old (cdr lat))))))

(defun subst (new old lat)
  (cond
   ((null lat) (quote ()))
   ((eq (car lat) old)
    (cons new (cdr lat)))
   (t (cons (car lat)
            (subst new old (cdr lat))))))

(defun subst2 (new o1 o2 lat)
  (cond
   ((null lat) (quote ()))
   ((or (eq (car lat) o1)
        (eq (car lat) o2))
    (cons new (cdr lat)))
   (t (cons (car lat)
            (subst2 new o1 o2 (cdr lat))))))


(defun multirember (a lat)
  (cond
   ((null lat) (quote ()))
   ((eq (car lat) a)
    (multirember a (cdr lat)))
   (t (cons (car lat)
            (multirember a (cdr lat))))))

(defun multiinsertR (new old lat)
  (cond
   ((null lat) (quote ()))
   ((eq (car lat) old)
    (cons old
          (cons new
                (multiinsertR new old (cdr lat)))))
   (t (cons (car lat)
            (multiinsertR new old (cdr lat))))))

(defun multiinsertL (new old lat)
  (cond
   ((null lat) (quote ()))
   ((eq (car lat) old)
    (cons new
          (cons old
                (multiinsertL new old (cdr lat)))))
   (t (cons (car lat)
            (multiinsertL new old (cdr lat))))))

(defun multisubst (new old lat)
  (cond
   ((null lat) (quote ()))
   ((eq (car lat) old)
    (cons new
          (multisubst new old (cdr lat))))
   (t (cons (car lat)
            (multisubst new old (cdr lat))))))

(provide '3-cons)

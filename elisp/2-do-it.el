;;; Do It, Do It Again, and Again, and Again...

(require '0-preface)

(defun lat? (l)
  "If l is a list of atom."
  (cond
   ((null l) t)
   ((atom? (car l)) (lat? (cdr l)))
   (t nil)))

(defun member? (a lat)
  "If a is one of the atoms of lat."
  (cond
   ((null lat) nil)
   (t (or (eq (car lat) a)
          (member? a (cdr lat))))))


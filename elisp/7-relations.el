;;; Friends and Relations

(require '0-preface)
(require '2-do-it)                      ; import member?
(require '3-cons)                       ; import multirember

(defun set? (lat)
  (cond
   ((null lat) t)
   ((member? (car lat) (cdr lat)) nil)
   (t (set? (cdr lat)))
   )
  )

(defun makeset (lat)
  (cond
   ((null lat) (quote ()))
   ((member? (car lat) (cdr lat))
    (makeset (cdr lat)))
   (t (cons (car lat)
            (makeset (cdr lat))))
   )
  )

(defun makeset (lat)
  (cond
   ((null lat) (quote ()))
   (t (makeset
       (multirember (car lat))
       (cdr lat)))
   )
  )

;; (defun subset? (set1 set2)
;;   (cond
;;    ((null set1) t)
;;    ((member? (car set1) set2)
;;     (subset? (cdr set1) set2))
;;    (t nil)
;;    )
;;   )

(defun subset? (set1 set2)
  (cond
   ((null set1) t)
   (t (and (member? (car set1) set2)
           (subset? (cdr set1) set2)))
   )
  )

(defun eqset? (set1 set2)
  (and (subset? set1 set2)
       (subset? set2 set1))
  )

;; (defun intersect? (set1 set2)
;;   (cond
;;    ((null set1) nil)
;;    ((member? (car set1) set2) t)
;;    (t (insersect? (cdr set1) set2))
;;    )
;;   )

(defun intersect? (set1 set2)
  (cond
   ((null set1) nil)
   (t (or (member? (car set1) set2)
          (intersect? (cdr set1) set2)))
   )
  )

(defun intersect (set1 set2)
  (cond
   ((null set1) (quote ()))
   ((member? (car set1) set2)
    (cons (car set1)
          (intersect (cdr set1) set2)))
   (t (intersect (cdr set1) set2))
   )
  )

(defun union (set1 set2)
  (cond
   ((null set1) set2)
   ((member? (car set1) set2)
    (union (cdr set1) set2))
   (t (cons (car set1)
            (union (cdr set1) set2)))
   )
  )

(defun difference (set1 set2)
  (cond
   ((null set1) (quote ()))
   ((member? (car set1) set2)
    (difference (cdr set1) set2))
   (t (cons (car set1)
          (difference (cdr set1) set2)))
   )
  )

(defun intersectall (l-set)
  (cond
   ((null (cdr l-set)) (car l-set))
   (t (intersect (car l-set)
                 (intersectall (cdr l-set))))
   )
  )

(defun a-pair? (x)
  (cond
   ((atom? x) nil)
   ((null x) nil)
   ((null (cdr x)) nil)
   ((null (cdr (cdr x))) t)
   (t nil)
   )
  )

(defun first (p)
  (car p)
  )

(defun second (p)
  (cadr p)
  )

(defun build (s1 s2)
  (cons s1
        (cons s2 (quote ())))
  )

(defun third (l)
  (caddr l)
  )

(defun fun? (rel)
  (set? (firsts rel))
  )

;; (defun revrel (rel)
;;   (cond
;;    ((null rel) (quote ()))
;;    (t (cons (build
;;              (second (car rel))
;;              (first (car rel)))
;;             (revrel (cdr rel))))
;;    )
;;   )

(defun revpair (pair)
  (build (second pair) (first pair))
  )

(defun revrel (rel)
  (cond
   ((null rel) (quote ()))
   (t (cons (revpair (car rel))
            (revrel (cdr rel))))
   )
  )

(defun seconds (l)
  (cond
   ((null l) (quote ()))
   (else (cons (second (car l))
               (seconds (cdr l))))))

(defun fullfun? (fun)
  (set? (seconds fun))
  )

(defun one-to-one? (fun)
  (fun? (revrel fun))
  )

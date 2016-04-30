;;; Numbers Games

(require '0-preface))

(defun o+ (n m)
  (cond
   ((zerop m) n)
   (t (add1 (o+ n (sub1 m))))))

(defun o- (n m)
  (cond
   ((zerop m) n)
   (t (sub1 (o- n (sub1 m))))))

(defun addtup (tup)
  (cond
   ((null tup) 0)
   (t (o+ (car tup) (addtup (cdr tup))))))

(defun o* (n m)
  (cond
   ((zerop m) 0)
   (t (o+ n (o* n (sub1 m))))))

(defun tup+ (tup1 tup2)
  (cond
   ((null tup1) tup2)
   ((null tup2) tup1)
   (t (cons (o+ (car tup1) (car tup2))
            (tup+ (cdr tup1) (cdr tup2))))))

(defun o> (n m)
  (cond
   ((zerop n) nil)
   ((zerop m) t)
   (t (o> (sub1 n) (sub1 m)))))

(defun o< (n m)
  (cond
   ((zerop m) nil)
   ((zerop n) t)
   (t (o< (sub1 n) (sub1 m)))))

;; (defun o= (n m)
;;   (cond
;;    ((zerop m) (zerop n))
;;    ((zerop n) nil)
;;    (t (o= (sub1 n) (sub1 m)))))

(defun o= (n m)
  (cond
   ((o> n m) nil)
   ((o< n m) nil)
   (t t)))

(defun o^ (n m)
  (cond
   ((zerop m) 1)
   (t (o* n (o^ n (sub1 m))))))

(defun o/ (n m)
  (cond
   ((o< n m) 0)
   (t (add1 (o/ (o- n m) m)))))

(defun quotient (n m)
  (values (truncate (o/ n m))))


(defun length (lat)
  (cond
   ((null lat) 0)
   (t (add1 (length (cdr lat))))))

(defun pick (n lat)
  (cond
   ((zerop (sub1 n)) (car lat))
   (t (pick (sub1 n) (cdr lat)))))

;; (defun rempick (n lat)
;;   (cond
;;    ((zerop (sub1 n)) (cdr lat))
;;    (t (cons (car lat)
;;             (rempick (sub1 n) (cdr lat))))))

(defun no-nums (lat)
  (cond
   ((null lat) (quote ()))
   ((numberp (car lat))
    (no-nums (cdr lat)))
   (t (cons (car lat) (no-nums (cdr lat))))))

(defun all-nums (lat)
  (cond
   ((null lat) (quote ()))
   ((numberp (car lat))
    (cons (car lat) (all-nums (cdr lat))))
   (t (all-nums (cdr lat)))))

(defun eqan? (a1 a2)
  (cond
   ((and (numberp a1) (numberp a2))
    (= a1 a2))
   ((or (numberp a1) (numberp a2))
    nil)
   (t (eq a1 a2))))

(defun occur (a lat)
  (cond
   ((null lat) 0)
   ((eq (car lat) a)
    (add1 (occur a (cdr lat))))
   (t (occur a (cdr lat)))))

(defun one? (n)
  (= n 1))

(defun rempick (n lat)
  (cond
   ((one? n) (cdr lat))
   (t (cons (car lat)
            (rempick (sub1 n) (cdr lat))))))

(provide '4-numbers)

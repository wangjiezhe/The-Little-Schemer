;;; Preface

(defun atom? (x)
    "If x is an atom.
An atom is a string of characters."
    (not (listp x)))

;; (defun add1 (x)
;;   (1+ x))

;; (defun sub1 (x)
;;   (1- x))

(defalias 'add1 '1+)

(defalias 'sub1 '1-)

(provide '0-preface)

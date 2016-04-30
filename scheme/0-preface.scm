;;; Preface

;;; If x is an atom.
;;; An atom is a string of characters.
(define (atom? x)
  (and (not (pari? x)) (not (null? x))))

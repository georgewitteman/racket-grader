;------------------------;
;;;;;;;;;;;;;;;;;;;;;;;;;-
;;;EXAMPLE STUDENT FILE;;-
;;;;;;;;;;;;;;;;;;;;;;;;;-
;------------------------;





(define student-name "Travers")

(define facty
  (lambda (x)
    (if (<= x 1)
        1
        (* x (facty (- x 1))))))

(define rev-listy
  (lambda (listy)
    (if (null? listy)
        '()
        (append (rev-listy (rest listy)) (list (first listy))))))

(define square
  (lambda (x)
    (* x 2)))


(define x-to-nth
  (lambda (x n)
    (if (<= n 0)
        1
        (* x (x-to-nth x (- n 1))))))
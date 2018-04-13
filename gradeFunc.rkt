<<<<<<< HEAD
<<<<<<< HEAD


;(define to-String
;  (lambda (datum)
;    datum
;     ))
;
;(define string-join
;  (lambda (listy)
;    (apply string-append (map  listy))))

=======
>>>>>>> 363ca1aff1cdd26d8c602c55331dc7888740717d
=======
>>>>>>> 363ca1aff1cdd26d8c602c55331dc7888740717d
(define grade
  (lambda (name pts func args soln)
    ;; Save the students solution
    (let* ([results (apply func args)] ;; Save students answers
           [fn-name (object-name func)] 
           [args-str (string-join args)
                  ])
      (cond
        ;; If student's answer is correct return full points
        ((equal? results soln)
         pts)
        ;; O/w print what went wrong and return 0 points
        (else
         (printf "~A - ERROR: ~A,~A --> ~A (~A)~%"name fn-name args-str results soln)
         0)
        ))))



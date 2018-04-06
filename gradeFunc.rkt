

(define grade
  (lambda (name pts func args soln)
    ;; Save the students solution
    (let* ([results (apply func args)] ;; Save students answers
           [fn-name (object-name func)] 
           [args-str (string-join
                      (map ~a args) " ")])
      (cond
        ;; If student's answer is correct return full points
        ((equal? results soln)
         pts)
        ;; O/w print what went wrong and return 0 points
        (else
         (printf "ERR: ~A,~A --> ~A (~A)" fn-name args-str results soln)
         0)
        )
      )
    )
  )
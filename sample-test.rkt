;------------------------;
;;;;;;;;;;;;;;;;;;;;;;;;;-
;;EXAMPLE INSTRUCT FILE;;-
;;;;;;;;;;;;;;;;;;;;;;;;;-
;------------------------;
(load "structs-and-helper-funcs.rkt")
(load "asserts.rkt")
(load "grader.rkt")

(define test-1
  (make-test "FACTY"
             '(2 2 2 2 2)
             10
             'facty 
             '((1) (2) (5) (6) (0))
             (letrec ((facty-soln (lambda (n)
                                    (if (<= n 1)
                                        1
                                        (* n (facty-soln (- n 1)))))))
               facty-soln)))

(define test-2
  (make-test "REV_LISTY"
             '(2 2 2 2)
             8
             'rev-listy
             '(((1 2 3 4)) (()) ((5 4 3)) ((6)))
             (letrec ((rev-listy-soln (lambda (listy)
                                        (if (null? listy)
                                            '()
                                            (append (rev-listy-soln (rest listy)) (list (first listy)))))))
               rev-listy-soln)))

(define my-asmt
  (make-asmt "ASMT-0" "CMPU-101" "4-21-2018" (list test-1 test-2)))
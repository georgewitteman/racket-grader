(define tests
  (list
   (make-test
    "Q1" "Add two zeros" 1
    (lambda ()
      (assert-eq? (doesntexist 0 0) 0)))
   (make-test
    "Q1" "Add a zero and a 1" 1
    (lambda ()
      (assert-eq? (+ 0 1) 1)
      (assert-eq? (+ 1 0) 0)))
   (make-test
    "Q1" "Add two 1's" 3
    (lambda ()
      (assert-eq? (+ 1 1) 2)))
   ))

(define division
  (lambda (x y)
    (/ x y)))

(define half
  (lambda (x)
    (/ x 2)))

(define test-suite
  (list
   (grade 'Q1' 1.5 division '(3 2) 1.5)
   (grade 'Q1' 0.5 division '(0 0) 0)
   (grade 'Q1' 2.0 half '(2) 1)))
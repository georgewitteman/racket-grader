#lang racket
(define (my-add a b c) (+ a b c))

(define (my-subtract a b c) (+ a (- b c)))

;; A test case
(struct case (myfn solnfn args))

;; Result of a test case
(struct result (case passed))

;; Assert with CHECK that the two input functions, MYFN and SOLNFN,
;; return the same value when applied to ARGS
(define (assert check myfn solnfn args)
  (check (apply myfn args) (apply solnfn args)))

;; Call ASSERT with CHECK equal to EQ?
(define (assert-eq? myfn solnfn args)
  (assert eq? myfn solnfn args))

;; Run a given test case
(define (test a-case)
  (result a-case (assert-eq? (case-myfn a-case) (case-solnfn a-case) (case-args a-case))))

;; Run a list of test cases
(define (test-suite tests)
  (map test tests))

;; Print results of test
(define (print-results results)
  (let* 
      ;; Calculate number of passed and failed tests 
      ([pass-fail
        (foldl (lambda (a-result results)
                 (cond
                   ;; Passed test
                   [(result-passed a-result)
                    ;; Increment passed counter
                    (list (+ 1 (first results)) (second results))]
                   ;; Failed test
                   [else
                    (let* ([a-case (result-case a-result)]
                           [myfn-name (object-name (case-myfn a-case))]
                           [args (case-args a-case)])
                      ;; Print info from test
                      (printf "Failed: ~a ~a~n" myfn-name (case-args a-case))
                      ;; Increment failed counter
                      (list (first results) (+ 1 (second results))))]))
               '(0 0)
               results)]
       ;; Calculate total number of tests
       [total (+ (first pass-fail) (second pass-fail))]
       [passed (first pass-fail)]
       [failed (second pass-fail)]
       [percent-correct (* (/ passed total) 100)])
    ;; Print test suite summary
    (printf "Ran ~a tests. ~a passed, ~a failed. Grade ~a%.~n"
            total
            passed
            failed
            percent-correct)))

(print-results
 (test-suite (list (case my-subtract - '(1 2 3))
                   (case my-subtract - '(0 0 0))
                   (case my-subtract - '(-1 1 -1))
                   (case my-subtract - '(0 0 1))
                   (case my-add + '(1 2 3))
                   (case my-add + '(0 0 0))
                   (case my-add + '(-1 1 -1))
                   (case my-add + '(0 0 1)))))

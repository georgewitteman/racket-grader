#lang racket
(define (add a b c) (+ a b c))

(define (subtract a b c) (+ a (- b c)))

;; A test case
(struct case (myfn args expect))

;; Result of a test case
(struct result (case passed))

(define (assert check myfn args expect)
  (check (apply myfn args) expect))

(define (assert-eq? myfn args expect)
  (assert eq? myfn args expect))

;; Run a given test case
(define (test a-case)
  (result a-case (assert-eq? (case-myfn a-case) (case-args a-case) (case-expect a-case))))

;; Run a list of test cases and return a list of their results
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
                           [args-str (anylist->string (case-args a-case))]
                           [my-result (apply (case-myfn a-case) (case-args a-case))]
                           [expected (case-expect a-case)])
                      ;; Print info from test
                      (printf "Failed: (~a ~a) => ~a (Solution: ~a)~n"
                              myfn-name
                              args-str
                              my-result
                              expected)
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

;; Helper to convert a list of any type into a string
(define (anylist->string lst)
  (string-join (map ~a lst) " "))

(print-results
 (test-suite (list (case subtract '(1 2 3) -4)
                   (case subtract '(0 0 0) 0)
                   (case subtract '(-1 1 -1) -1)
                   (case subtract '(0 0 1) -1)
                   (case add '(1 2 3) 6)
                   (case add '(0 0 0) 0)
                   (case add '(-1 1 -1) -1)
                   (case add '(0 0 1) 1))))

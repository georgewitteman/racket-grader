;; A test case
(struct case (myfn args expect))

;; Result of a test case
(struct result (case passed))

(define assert
  (lambda (check myfn args expect)
    (check (apply myfn args) expect)))

(define assert-eq?
  (lambda (myfn args expect)
    (assert eq? myfn args expect)))

;; Run a given test case
(define test
  (lambda (a-case)
    (result a-case (assert-eq? (case-myfn a-case)
                               (case-args a-case)
                               (case-expect a-case)))))

;; Run a list of test cases and return a list of their results
(define test-suite
  (lambda (tests)
    (map test tests)))

;; Print results of test
(define print-results
  (lambda (student results)
    (printf "~a:~n-----------------------------------~n" student)
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
                             [args-str (string-join
                                        (map ~a (case-args a-case)) " ")]
                             [my-result (apply (case-myfn a-case)
                                               (case-args a-case))]
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
      (printf "Ran ~a tests. ~a passed, ~a failed. Grade ~a%.~n~n"
              total
              passed
              failed
              percent-correct))))
;; Struct for results of tests
;; NAME: Name of the test
;; MAX: Max points student could have received
;; PTS: Points the student received
;; MSG: An optional error message
(define-struct result (name max pts msg))

(define-struct test (name max func))

(define assert-eq?
  (lambda (v1 v2)
    (if (eq? v1 v2)
        #t
        (error "assert-eq? fail"))))

(define run-test
  (lambda (testy)
    (let ([resulty (make-result
                    (test-name testy)
                    (test-max testy)
                    0
                    empty)])
      (with-handlers
          ([exn:fail?
            (lambda (exn)
              (make-result
               (test-name testy)
               (test-max testy)
               0
               (exn-message exn)))])
        ((test-func testy))
        (make-result
         (test-name testy)
         (test-max testy)
         (test-max testy)
         empty)))))

(define print-result
  (lambda (res)
    (printf "~A (~A/~A)~A~n"
            (result-name res)
            (result-pts res)
            (result-max res)
            (if (empty? (result-msg res))
                ""
                (string-append ": " (result-msg res))))))

;; Main function that grades all the assignments
;; Searches through the current directory for all files of the
;; form <username>-ASMT-NAME.rkt, loads them, runs the tests,
;; and 
(define grade-asmt
  (lambda (asmt-name test-file)
    (for-each
     ;; Load and run tests on each assignment file
     (lambda (filename)
       (load test-file)
       (load filename)
       (printf "~a:~n-----------------------------------~n" filename)
       (map print-result (map run-test tests))
       (printf "~n"))
     ;; Get a list of all the assignment files in
     ;; the current directory
     (filter
      (lambda (file)
        (regexp-match?
         (regexp (string-append "-" asmt-name ".rkt"))
         file))
      (directory-list ".")))))
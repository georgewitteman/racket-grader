(load "asserts.rkt" )
;; STRUCT: RESULT
;; gives results of tests
;;--------------
;; NAME-OF-TEST: String, name of the test for which we have results
;; INPUT-VEC: List, a list of lists containing the inputs for each trial
;; RESULTS-VEC: List, a list containing the results of applying students func to input-vec
;; SOLN-VEC: List, a list containing the results of applying soln func to input-vec
;; POINTS-VEC: List, containing either 0 or some other number
;; MSG: string, message for any possible errors encountered
(define-struct result (name-of-test
                       input-vec
                       results-vec
                       soln-vec
                       pts-vec
                       msg))

;; STRUCT: TEST
;;------------
;; NAME-OF-TEST: String, name of the test
;; POINTS-PER-CASE: Number, points received per correct result
;; FUNC-NAME: Symbol, name of test as it will be in students files
;; LIST-O-INPUTS: List, a list of lists containing the inputs for each trial
;; SOLN-FUNCTION: Symbol, func defined to give correct solns
(define-struct test (name-of-test
                     points-per-case
                     func-name
                     list-o-inputs
                     soln-function))

;; STRUCT: ASMT
;;------------
;; NUMBER: A string, the asmt number, such as "ASMT 1"
;; CLASS: A string, the class the asmt if for, such as "CMPU 101"
;; DATE: A string, the current date
;; LIST-O-TESTS: A list, list of tests for the asmt
(define-struct asmt (number class date list-o-tests))

;;; SUMLIST
;;;--------
;;; function that returns the sum of elements in a list
(define sumlist
  (lambda (listy)
    (if (null? listy)
        0
        (+ (first listy) (sumlist (rest listy))))))


;; -------------------------------------------------------

(define run-test
  (lambda (testy)
    (with-handlers
        ([exn:fail?
          (lambda (exn)
            (make-result
             (test-question testy)
             (test-name testy)
             (test-max testy)
             0
             (exn-message exn)))])
      ((test-func testy))
      (make-result
       (test-question testy)
       (test-name testy)
       (test-max testy)
       (test-max testy)
       empty))))


;;; PRINT-RESULT
;;;-------------
;;; prints result for one test, over possibly multiple inputs
(define print-result
  (lambda (res)
    (printf "Test ~A:~n  Expected Output   Student Output   Points"
            (result-name-of-test res))
    (let* ([len (length (result-input-vec ))]
           [inputs (result-input-vec res)]
           [results (result-results-vec res)]
           [soln (result-soln-vec res)]
           [points (result-pts-vec res)])
      (dotimes (i len)
               (printf "input(s): ~A" (list-ref inputs i))
               (printf "      ~A" (list-ref results i))
               (printf "      ~A" (list-ref soln i))
               (printf "      ~A~n" (list-ref points i))
               )
      (printf "-------------------~n")
      (printf "SUBTOTAL: ~A~n" (sumlist points)))

            (if (empty? (result-msg res))
                ""
              (string-append ": " (result-msg res)))))




(define asmt-1 (make-asmt "ASMT-1" "CMPU-101" "4/18/2018" '((2 3) (1 3) (2 3))))

;;; Prints all results for a given student
(define print-all-results
  (lambda (student-name my-asmt)
    (printf "~n-------------------~nSTUDENT: ~A~n" student-name)
    (printf "ASMT-INFO: ~A, ~A, DATE: ~A ~n" (asmt-number my-asmt) (asmt-class my-asmt) (asmt-date my-asmt))
    (printf "-------------------")
    (let* ([listy (asmt-list-o-tests my-asmt)]
           [length-listy (length listy)]
           [result-list (map run-test listy)])
      (map print-result result-list)
      (printf "TOTAL SCORE: "
      ))))

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
       (map print-result (map run-test tests)) ;; Returns list of result structs ;;;
       (printf "~n"))
     ;; Get a list of all the assignment files in
     ;; the current directory
     (filter
      (lambda (file)
        (regexp-match?
         (regexp (string-append "-" asmt-name ".rkt"))
         file))
      (directory-list ".")))))

;;-----------------------------------------------------;;
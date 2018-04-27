
;;;;;;;;;;;;;;;;;;;;;
;;;----STRUCTS----;;;
;;;;;;;;;;;;;;;;;;;;;
;;;---------------;;;


;; STRUCT: RESULT
;; gives results of tests
;;--------------
;; NAME-OF-TEST: String, name of the test for which we have results
;; INPUT-VEC: List, a list of lists containing the inputs for each trial
;; RESULTS-VEC: List, a list containing the results of applying students func to input-vec
;; SOLN-VEC: List, a list containing the results of applying soln func to input-vec
;; POINTS-VEC: List, containing either 0 or some other number
(define-struct result (name-of-test
                       input-vec
                       results-vec
                       soln-vec
                       pts-vec
                       ))

;; STRUCT: TEST
;;------------
;; NAME-OF-TEST: String, name of the test
;; POINTS-PER-CASE: LIST, containing the corresponding points each case is worth
;; MAX-PTS: Number, max points received for a question
;; FUNC-NAME: Symbol, name of test as it will be in students files
;; LIST-O-INPUTS: List, a list of lists containing the inputs for each trial
;; SOLN-FUNCTION: Symbol, func defined to give correct solns

(define-struct test (name-of-test
                     points-for-case
                     max-pts
                     func-name
                     list-o-inputs
                     soln-function
                     ))

;; STRUCT: ASMT
;;------------
;; NUMBER: A string, the asmt number, such as "ASMT 1"
;; CLASS: A string, the class the asmt if for, such as "CMPU 101"
;; DATE: A string, the current date
;; LIST-O-TESTS: A list, list of tests for the asmt
(define-struct asmt (number class date list-o-tests))

;;;;;;;;;;;;;;;;;;;;;;
;;;HELPER FUNCTIONS;;;
;;;;;;;;;;;;;;;;;;;;;;


;;; 
(define replaceNth
  (lambda (nth item list1)
    (cond [(= nth 1) (cons item (cdr list1))] ;; If nth = 1 replace element at that location and add rest of list
          [else (cons (car list1) (replaceNth (- nth 1) item (cdr list1)))]))) ;else decrement nth, and cons/ recursively call replaceNth on rest of list


;;; SUMLIST
;;;--------
;;; function that returns the sum of elements in a list
(define sumlist
  (lambda (listy)
    (if (null? listy)
        0
        (+ (first listy) (sumlist (rest listy))))))
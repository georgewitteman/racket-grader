(load "format.rkt")
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
                       pts-vec))

;; STRUCT: TEST
;;------------
;; NAME-OF-TEST: String, name of the test
;; POINTS-PER-CASE: LIST, containing the corresponding points each case is worth
;; MAX-PTS: Number, max points received for a question
;; FUNC-NAME: Symbol, name of test as it will be in students files
;; LIST-O-INPUTS: List, a list of lists containing the inputs for each trial
;; PRED: Predicate function to check equality of solution and student output
;; SOLN-FUNCTION: Symbol, func defined to give correct solns

(define-struct test (name-of-test
                     points-for-case
                     max-pts
                     func-name
                     list-o-inputs
                     pred
                     soln-function))

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


;; REPLACE-NTH
;; -----------------
;; Returns a list with the NTH element in LIST1 replaced with ITEM
(define replace-nth
  (lambda (nth item list1)
    (cond
      ;; If nth = 1 replace element at that location and add rest of list
      [(= nth 1) (cons item (cdr list1))]
      ;else decrement nth, and cons/ recursively call replace-nth on rest of list
      [else (cons (car list1) (replace-nth (- nth 1) item (cdr list1)))]))) 


;; SUMLIST
;; --------
;; Return the sum of the elements in LISTY
(define sumlist
  (lambda (listy)
    (if (null? listy)
        0
        (+ (first listy) (sumlist (rest listy))))))


;; SUBSTRINGY
;; --------------------
;; Make a string of LENN from IN-STR
(define substringy
  (lambda (in-str lenn)
    (let ((str (format "~A" in-str))) ;; Convert IN-STR to string
      (format
       ;; Build the format string (e.g. "~10F") to enforce minimum length
       (string-append "~" (number->string lenn) "F")
       ;; Get substring to enforce maximum length
       (substring str 0 (min (string-length str) lenn))))))
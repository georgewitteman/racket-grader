;; Struct for results of tests
;; NAME: Name of the test
;; MAX: Max points student could have received
;; PTS: Points the student received
;; MSG: An optional error message
(define-struct result (question name max pts msg))

(define-struct test (question name max func))

(define-struct asmt (number class date list-o-tests))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;-------------ASSERTS--------------;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define assert-eq?
  (lambda (v1 v2)
    (if (eq? v1 v2)
        #t
        (error "assert-eq? fail"))))

;; 
;; Inputs: X, the user input
;;         l, the lower bound of the range
;;         u, the upper bound of the range
;;  Note that the interval is inclusive
(define assert-in-range?
  (lambda (x l u)
    (if (<= l x u)
        #t
        (error "assert-in-range? fail"))))

(define assert-within-delta?
  (lambda (x delta)
    (if (<= (- x delta) x (+ x delta))
        #t
        (error "assert-within-delta? fail"))))

;; Inputs: X, the user input
;;         K, the value X must be less than
(define assert-less-than?
  (lambda (x k)
    (if (< x k)
        #t
        (error "assert-less-than? fail"))))

;; Less-than-or-eq
;; Inputs: X, the user input
;;         K, the value X must be less than
(define assert-leq-than?
  (lambda (x k)
    (if (<= x k)
        #t
        (error "assert-leq-than? fail"))))

;; Inputs: X, the user input
;;         K, the value X must be less than
(define assert-greater-than?
  (lambda (x k)
    (if (> x k)
        #t
        (error "assert-greater-than? fail"))))

;; Greater-than-or-eq
;; Inputs: X, a number, the user input
;;         K, a number, the value X must be less than
(define assert-geq-than?
  (lambda (x k)
    (if (>= x k)
        #t
        (error "assert-geq-than? fail"))))

;; Inputs:  listy, the user's input list
;;          listz, another specified list
;; Returns #t if listy  is the same as listz
;; as judged by equal? 
(define assert-same-list?
  (lambda (listy listz)
    (if (equal? listy listz)
        #t
        (error "assert-same-list? fail"))))

;; Inputs:  listy, the user's input list
;;          listz, another specified list
;; Returns #t if listy  is the same as listz
;; as judged by equal? or if (reverse listy) is the same as
;; listz as judged by equal?
(define assert-same-list-rev?
  (lambda (listy listz)
    (if (or (equal? listy listz)
            (equal? (reverse listy) listz))
        #t
        (error "assert-same-list-rev? fail"))))

(define assert-string?
  (lambda (x)
    (if (string? x)
        #t
        (error "assert-string? fail"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;-------------------------------;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



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




;;(define print-result
  ;;(lambda (res)
    ;;(printf "~A: ~A (~A/~A)~A~n"
     ;;       (result-question res)
       ;;     (result-name res)
         ;;   (result-pts res)
           ;; (result-max res)
           ;; (if (empty? (result-msg res))
             ;;   ""
              ;;  (string-append ": " (result-msg res))))))
(define asmt-1 (make-asmt "ASMT-1" "CMPU-101" "4/18/2018" '(1 2 3 4)))

(define print-result
  (lambda (student-name my-asmt)
    (printf "-------------------~nSTUDENT: ~A~n" student-name)
    (printf "ASMT-INFO: ~A, ~A, DATE: ~A ~n" (asmt-number my-asmt) (asmt-class my-asmt) (asmt-date my-asmt))
    (printf "-------------------")
    (let* ([listy (asmt-list-o-tests my-asmt)]
           [length-listy (- (length listy) 1)])
      (dotimes (i length-listy)
            (let* ([cur-test (list-ref listy i)]
                   [test-name (test-name cur-test)]
                   [points-per-case (test-points-per-case cur-test)]
                   [func-name (test-function-name cur-test)]
                   [input-list (test-list-o-inputs cur-test)]
                   [soln-func (test-soln-func cur-test)])
              
              (printf "hey")
              )   
          )
      )
    ))

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
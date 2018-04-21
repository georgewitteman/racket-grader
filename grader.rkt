(load "asserts.rkt")
(load "structs-and-helper-funcs.rkt")
(require scheme/pretty)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;-------------------------------;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Run individual case
(define run-case
  (lambda (func)
    (lambda (inputs)
      (with-handlers
          ([exn:fail?
            (lambda (exn)
            exn)])
        (apply (eval func) inputs)))))


;; -------------------------------------------------------

(define run-test
  (lambda (testy)
    (let* ([res (make-result (test-name-of-test testy)
                            (test-list-o-inputs testy)
                            (map (run-case (test-func-name testy)) (test-list-o-inputs testy))
                            (map (lambda (inputs) (apply (test-soln-function testy) inputs)) (test-list-o-inputs testy))
                            (map (lambda (x) 0) (test-list-o-inputs testy)))]
          [len (length (test-list-o-inputs testy))]
          [stu-res (result-results-vec res)]
          [soln-res (result-soln-vec res)]
          [points-per (test-points-for-case testy)]
          [res-pts (result-pts-vec res)])
      (dotimes (i len)
               (if (equal? (list-ref stu-res i) (list-ref soln-res i))
                   (set-result-pts-vec! res (replaceNth (+ i 1) (list-ref points-per i) (result-pts-vec res)))))
      res)))


;;; PRINT-RESULT
;;;-------------
;;; prints result for one test, over possibly multiple inputs
(define print-result
  (lambda (res)
    (printf "Test ~A:~n                        Expected Output   Student Output   Points"
            (result-name-of-test res))
    (let* ([len (length (result-input-vec res))]
           [inputs (result-input-vec res)]
           [results (result-results-vec res)]
           [soln (result-soln-vec res)]
           [points (result-pts-vec res)])
      (dotimes (i len)
               (printf "~ninput(s): ~A" (first (list-ref inputs i)))
               (printf "      ~A" (list-ref soln i))
               (printf "          ~A" (list-ref results i))
               (printf "          ~A" (list-ref points i))
               )
      (printf "~n-------------------~n")
      (printf "SUBTOTAL: ~A~n" (sumlist points))
      (printf "-------------------~n"))))


;;; Prints all results for a given student
(define print-all-results
  (lambda (my-asmt)
    (printf "~n-------------------~nSTUDENT: ~A~n" student-name)
    (printf "ASMT-INFO: ~A, ~A, DATE: ~A ~n" (asmt-number my-asmt) (asmt-class my-asmt) (asmt-date my-asmt))
    (printf "-------------------~n")
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
       (print-all-results student-name cur-asmt) ;; Returns list of result structs ;;;
       (printf "~n")
       ;;CODE HERE ---to set functions to null)
       )
     
     ;; Get a list of all the assignment files in
     ;; the current directory
     (filter
      (lambda (file)
        (regexp-match?
         (regexp (string-append "-" asmt-name ".rkt"))
         file))
      (directory-list ".")))))

;;-----------------------------------------------------;;

(define asmt-1 (make-asmt "ASMT-1" "CMPU-101" "4/18/2018" (list (make-test "TESTY" '(1 2) (lambda (listy) (first listy)) '((()) ((1 2 3 4))) (lambda (listy) listy))
                                                                (make-test "TEST 2" '(3 4) * '((1 2) (1 2 3 4)) *))))
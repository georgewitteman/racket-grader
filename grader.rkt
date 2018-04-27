(load "format.rkt")
(load "asserts.rkt")
(load "structs-and-helper-funcs.rkt")
(require scheme/pretty)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;-------------------------------;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Total possible points
(define *TOTAL* 0)

;; RUN-CASE
;; ----------------------
;; Run individual case. Returns a function which takes inputs and evaluates
;; the input FUNC on INPUTS. If any exceptions are raised return the EXN
(define run-case
  (lambda (func)
    (lambda (inputs)
      ;; Handle errors
      (with-handlers
          ([exn:fail?
            (lambda (exn)
              exn)])
        (apply (eval func) inputs)))))

;; RUN-TEST
;; --------------------
;; Run all tests for a question
(define run-test
  (lambda (testy)
    (let* ([res (make-result (test-name-of-test testy) ;; Name of test
                             (test-list-o-inputs testy) ;; Inputs to test
                             ;; Student Results
                             (map (run-case (test-func-name testy))
                                  (test-list-o-inputs testy))

                             ;; Soln Results
                             (map (lambda (inputs)
                                    (apply (test-soln-function testy) inputs))
                                  (test-list-o-inputs testy))

                             ;; Points vector
                             (map (lambda (x) 0) (test-list-o-inputs testy)))]
           [len (length (test-list-o-inputs testy))]
           [stu-res (result-results-vec res)]
           [soln-res (result-soln-vec res)]
           [points-per (test-points-for-case testy)]
           [res-pts (result-pts-vec res)])
      ;; Check to see if the student received the correct answer and if they did
      ;; give them the points for the question
      (dotimes (i len)
               (if (equal? (list-ref stu-res i) (list-ref soln-res i))
                   (set-result-pts-vec! res (replace-nth (+ i 1)
                                                        (list-ref points-per i)
                                                        (result-pts-vec res)))))
      (set! *TOTAL* (+ (sumlist (result-pts-vec res)) *TOTAL*))
      res)))

;;; PRINT-RESULT
;;;-------------
;;; prints result for one test, over possibly multiple inputs
(define print-result
  (lambda (res)
    (printf "Test ~A~n" (result-name-of-test res))
    (printf "\t\t     | Expected Output |  Student Output |          Points~n")
    (printf "-------------------------------------")
    (printf "-------------------------------------")
    (let* ([len (length (result-input-vec res))]
           [inputs (result-input-vec res)]
           [results (result-results-vec res)]
           [soln (result-soln-vec res)]
           [points (result-pts-vec res)])
      (dotimes (i len)
               (printf "~ninput(s): ~A"
                       (substringy (first (list-ref inputs i)) 10))
               (printf " | ~A" (substringy (list-ref soln i) 15))
               (printf " | ~A" (substringy (list-ref results i) 15))
               (printf " | ~A" (substringy (list-ref points i) 15)))
      (printf "~n-------------------~n")
      (printf "SUBTOTAL: ~A~n" (sumlist points))
      (printf "-------------------~n"))))

;; PRINT-ALL-RESULTS
;; ---------------------------
;; Prints all results for a given student
(define print-all-results
  (lambda (my-asmt student-name)
    (printf "~n-------------------~nSTUDENT: ~A~n" student-name)
    (printf "ASMT-INFO: ~A, ~A, DATE: ~A ~n"
            (asmt-number my-asmt)
            (asmt-class my-asmt)
            (asmt-date my-asmt))
    (printf "-------------------~n")
    (let* ([listy (asmt-list-o-tests my-asmt)]
           [length-listy (length listy)]
           [result-list (map run-test listy)])
      (map print-result result-list)
      (printf "TOTAL SCORE: ~A/~A" *TOTAL* (find-total-poss-points)))))

;; GRADE-ASMT
;; -------------------------------------------------------------
;; INPUTS: ASMT-NAME: string, name of the assignment (e.g. "asmt0")
;;         TEST-FILE: string, filename of the instructor's tests
;;
;; Main function that grades all the assignments
;; Searches through the current directory for all files of the
;; form <username>-ASMT-NAME.rkt, loads them, runs the tests,
;; and displays the results of the test
(define grade-asmt
  (lambda (asmt-name test-file)
    (let ((list-o-files (filter (lambda (file)
                                  (regexp-match?
                                   (regexp (string-append "-" asmt-name ".rkt"))
                                   file))
                                (directory-list "."))))
      (dolist (filename list-o-files)
              (load test-file)
              (load filename)
              (printf "~nFilename: ~a" filename)
              (print-all-results my-asmt student-name)
              (printf "~n")
              (printf "~n~n====================================")
              (printf "=======================================~n~n")
              (reset-global)
              (set! *TOTAL* 0)))))

;; RESET-GLOBAL
;; -----------------------
;; Reset the global environment to remove the students functions.
;;
;; Students may forget to define certain functions so we want to make sure they
;; don't get credit for functions that they do not define (but are defined by
;; other students).
(define reset-global
  (lambda ()
    (let ([list-o-tests (asmt-list-o-tests my-asmt)])
      (dolist (testy list-o-tests)
              (eval (list 'define (test-func-name testy) #f))))))

;; FIND-TOTAL-POSS-POINTS
;; ----------------------------
;; Returns the total points that are possible to receive on a given assignment.
(define find-total-poss-points
  (lambda ()
    (let ([list-o-tests (asmt-list-o-tests my-asmt)]
          [points 0])
      (dolist (testy list-o-tests)
              (set! points (+ points (test-max-pts testy))))
      points)))
              

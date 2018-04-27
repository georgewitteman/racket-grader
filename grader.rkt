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
      ;; Error handling
      (with-handlers
          ([exn:fail?
            (lambda (exn)
              exn)])
        (apply (eval func) inputs)))))


;; -------------------------------------------------------

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
                   (set-result-pts-vec! res (replaceNth (+ i 1)
                                                        (list-ref points-per i)
                                                        (result-pts-vec res)))))
      res)))


;;; PRINT-RESULT
;;;-------------
;;; prints result for one test, over possibly multiple inputs
(define print-result
  (lambda (res)
    (printf "Test ~A:~n\t\t\tExpected Output\tStudent Output\tPoints"
            (result-name-of-test res))
    (let* ([len (length (result-input-vec res))]
           [inputs (result-input-vec res)]
           [results (result-results-vec res)]
           [soln (result-soln-vec res)]
           [points (result-pts-vec res)])
      (dotimes (i len)
               (printf "~ninput(s): ~A" (first (list-ref inputs i)))
               (printf "\t\t~A" (list-ref soln i))
               (printf "\t\t~A" (list-ref results i))
               (printf "\t\t~A" (list-ref points i))
               )
      (printf "~n-------------------~n")
      (printf "SUBTOTAL: ~A~n" (sumlist points))
      (printf "-------------------~n"))))

;;; Prints all results for a given student
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
      (printf "TOTAL SCORE: "))))

;; Main function that grades all the assignments
;; Searches through the current directory for all files of the
;; form <username>-ASMT-NAME.rkt, loads them, runs the tests,
;; and
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
              (reset-global)
              (printf "~n")))))

; (define funcs '(facty abs list-to-veck))
; (eval (list 'define (first funcs) #f))
    ;;-----------------------------------------------------;;

(define reset-global
  (lambda ()
    (let ([list-o-tests (asmt-list-o-tests my-asmt)])
      (dolist (testy list-o-tests)
              (eval (list 'define (test-func-name testy) #f))))))

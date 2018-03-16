#lang racket
(define (my-add a b) (+ a b))

(define (my-subtract a b) (+ a b))

; c: the function that checks for equality of the return values
; t: the function to test
; s: the solution function
; . args: the arguments to pass to both functions
(define (assert c t s . args)
  (c (apply t args) (apply s args)))

; t: the function to test
; s: the solution function
; args: a list of arguments to pass to both functions
(define (assert-eq? t s args)
  (apply assert (append (list eq? t s) args)))

; t: the function to test
; s: the solution function
; args: a list of lists of arguments to pass to the functions (test cases)
; returns #t if all the tests passed, #f if at least one failed
(define (test t s args)
  (test-help t s args null null))

; t: the function to test
; s: the solution function
; args: a list of lists of arguments to pass to the functions (test cases)
; passed: a list of the tests that passed
; failed: a list of the tests that failed
; returns #t if all the tests passed, #f if at least one failed
(define (test-help t s args passed failed)
  (cond
    ;; No more test cases
    [(empty? args) (print-summary passed failed)
                   (= (length failed) 0)]
    ;; Tests passed
    [(assert-eq? t s (first args)) (test-help
                                    t
                                    s
                                    (rest args)
                                    (cons (first args) passed)
                                    failed)]
    ;; Tests failed
    [else (test-help t s (rest args) passed (cons (first args) failed))]))

; passed: a list of the tests that passed
; failed: a list of the tests that failed
(define (print-summary passed failed)
  (map (lambda (test) (printf "Passed test: ~a~n" test)) passed)
  (map (lambda (test) (printf "Failed test: ~a~n" test)) failed)
  (printf
   "Finished running ~a tests. Passed: ~a. Failed ~a.~n"
   (+ (length passed) (length failed))
   (length passed)
   (length failed)))

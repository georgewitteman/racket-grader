#lang racket/load

(load "lib.rkt")
(define asmt-regex #rx"-asmt0.rkt$")

(for-each
 ;; Load and run tests on each assignment file
 (lambda (filename)
   (load filename)
   (print-results
    filename
    ;; The actual test suite
    (test-suite (list (case subtract '(1 2 3) -4)
                      (case subtract '(0 0 0) 0)
                      (case subtract '(-1 1 -1) -1)
                      (case subtract '(0 0 1) -1)
                      (case add '(1 2 3) 6)
                      (case add '(0 0 0) 0)
                      (case add '(-1 1 -1) -1)
                      (case add '(0 0 1) 1)))))
 ;; Get a list of all the assignment files in
 ;; the current directory
 (filter
  (lambda (file)
    (regexp-match? asmt-regex file))
  (directory-list ".")))

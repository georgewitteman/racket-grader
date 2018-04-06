(define asmt-regex #rx"-asmt0.rkt$")
(load "tests.rkt") ;; Loads in list of TESTS 

(define grade-assmt
  (lambda (asmt-name test-file)
    (for-each
     ;; Load and run tests on each assignment file
     (lambda (filename)
       (load filename)
       
       ;; Get a list of all the assignment files in
       ;; the current directory
       (filter
        (lambda (file)
          (regexp-match? asmt-regex file))
        (directory-list ".")))))
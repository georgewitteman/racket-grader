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


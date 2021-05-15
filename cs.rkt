#lang racket

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (make_rat n d) (cons n d))
(define (make_mix_rat p q r) (list p q r))

; Checks if given data is pure fraction
(define (check_rat? f)
  (cond ((number? f) #f)
        ((pair? (cdr f)) #f) 
        (else #t))
  )

; Checks if given data is mixed fraction
(define (mixed_rat? f)
  (list? f)
  )
  
; To check sign of integer
(define (sgn x)
    (cond ((= x 0) 0)
          ((> x 0) 1)
          (else -1)))

; Handles Negative numbers as well
(define (simplify_fraction number)
  (let ([an (abs (numer number))]
        [ad (abs (denom number))])
    (let ([g (gcd an ad)])
      (cond ((= 0 an) (cons 0 1))
          ((= (sgn (numer number))
              (sgn (denom number)))
           (cons (/ an g) (/ ad g)))
          (else (cons (* -1 (/ an g))
                      (/ ad g)))))))

;Converts all types of fractions/numbers to P divide by Q form
(define (conv_fraction x)
  (cond ((number? x) (make_rat x 1))
      ((mixed_rat? x)
         (mixed_to_normal
           (make_mix_rat (car x)
                         (car (complex_to_simple (cadr x) (caddr x)))
                         (cdr (complex_to_simple (cadr x) (caddr x))))))
      (else (complex_to_simple (car x) (cdr x))))
)


(define (conv x)
  (print (simplify_fraction (cons (truncate (* x 1000)) 1000))))

(define (add-rat x y)
  (make_rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make_rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make_rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make_rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (^ x y) 
  (let ([b (conv_fraction x)]
        [e (conv_fraction y)])
  ( let ([ base  (exact->inexact (/ (numer b) (denom b) ) ) ]
           [ exponent  (exact->inexact (/ (numer e) (denom e) ) ) ])
           (conv (expt base exponent))) 
))

(define (inv x)
  (let ([o (conv_fraction x)])
    (cons (denom o) (numer o))))


;Handles negative mixed fractions as well like -1(2/3) returns (-5/3)
(define (mixed_to_normal x)
  (cond ((not (= (sgn (car x)) -1))
         (make_rat (+ (* (car x) (caddr x)) (cadr x) )
                   (caddr x)))
        (else
         (make_rat (* (- 1) (+ (* (* -1 (car x)) (caddr x)) (cadr x) ))
                   (caddr x)))
  ))

; complex_to_simple Complex fractions. Handles all cases.
(define (complex_to_simple p q)
  (cond ((and (number? p) (number? q) ) (make_rat p q))
        ((and (not (number? p)) (number? q) )
         (make_rat (car (complex_to_simple (car p) (cdr p)))
                   (* (cdr (complex_to_simple (car p) (cdr p))) q)))
        ((and (number? p) (not (number? q)))
         (make_rat (* p (cdr (complex_to_simple (car q) (cdr q))))
                   (car  (complex_to_simple (car q) (cdr q))) ))
        ((and (not (number? p)) (not (number? q))) 
          (make_rat 
            (* (car (complex_to_simple (car p) (cdr p)))
               (cdr (complex_to_simple (car q) (cdr q))))
            (* (cdr (complex_to_simple (car p) (cdr p)))
               (car (complex_to_simple (car q) (cdr q))))
          )
        )
  )
)

; Handles the arithmetic operaration 
(define (cal operation x y)
  (cond ((eq? operation +) (add-rat x y))
        ((eq? operation -) (sub-rat x y))
        ((eq? operation *) (mul-rat x y))
        ((eq? operation /) (if (eq? (* (denom x) (numer y)) 0)
                               (error "Divide by Zero!!!")
                               (div-rat x y)))
        ((eq? operation =) (equal-rat? x y))
        ((eq? operation ^) (^ x y))
        ))
         
; main function to receive the inputs
(define (calc operation operand1 operand2)
  (let ([op1 (simplify_fraction (conv_fraction operand1))]
        [op2 (simplify_fraction (conv_fraction operand2))])
        (simplify_fraction (cal operation op1 op2))
    ))

;Print the value in fraction form
(define (print x)
  (display (numer x))
  (display "/")
  (display (denom x)))

;(conv_fraction (make_rat 1 2))
;(conv_fraction (make_mix_rat 1 2 3))
;(print (calc + (make_mix_rat 1 2 3) (make_rat 1 2)))

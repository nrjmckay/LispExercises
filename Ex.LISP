;Exercise #1
;part A
;a. (A (B C)) ;b. (D (E F)) ;c. ((B C))
;d. (E F) ;e. (P O L) 
;part B (I used print just so I see the output)
(print (car (cdr (cdr (cdr '(A B C D)))))) ;a.
(print (car (cdr (car (cdr '(A (B C ) E)))))) ;b.
(print (car (car (car '(((D) E) U))))) ;c.
(print (car (cdr (car '(((D) E) U))))) ;d.

;exercise 2
(defun elementIsNumber (L)
    (if (numberP(car (cdr L)))
        t
        nil))


;exercsie 3
(defun elementIsList (L)
(if (consp(car (cdr L)))
  t
  nil))


;exercise 4
(defun base8 (n)
    (let* ((x (/ n 8)))
          (cond
              ((< n 8) (cons n '()))
              (t(append (base8 (floor (/ n 8)))(list (- n (* (floor x) 8)))))
              )))


;exercise 5
(defun mymember (x lst)
    (cond
        ((null lst) nil)
        ((eql x (car lst)) t)
        (t(mymember x (cdr lst)))
        ))


;exercise 6
(defun nbDigits (n)
    (cond
        ((< n 10) 1)
        (t(+ 1 (nbdigits (/ n 10))))
        ))


;exercise 7
(defun binary_length (n)
    (cond
        ((or (zerop n) (= n 1)) 1)
        (t(+ 1 (binary_length (floor (/ n 2)))))
        ))


;exercise 8
(defun binary_list (n)
    (let* ((x (/ n 2)))
          (cond
              ((< n 1) nil)
              ((< n 2) (cons n '()))
              (t(append (binary_list (floor (/ n 2)))(list (- n (* (floor x) 2)))))
              )))


;exercise 9
(defun nth2 (n lst)
    (cond
        ((= 1 n) (car lst))
        (t( nth2 (- n 1) (cdr lst)))
        ))


;exercise 10
(defun nthcdr2 (n lst)
    (cond
        ((zerop n) lst)
        (t(nthcdr2 (- n 1) (cdr lst)))
        ))


;exercise 11
(defun nthcar2 (n lst)
    (cond
        ((zerop n) '())
        (t(cons (car lst) (nthcar2 (- n 1) (cdr lst))))
        ))
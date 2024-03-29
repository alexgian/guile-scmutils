;#| -*-Scheme-*-
;
;$Id: interp.scm,v 1.2 2013/02/15 14:46:49 gildea Exp $
;
;Copyright 2005 Massachusetts Institute of Technology
;
;This file is part of MIT/GNU Scheme.
;
;MIT/GNU Scheme is free software; you can redistribute it and/or modify
;it under the terms of the GNU General Public License as published by
;the Free Software Foundation; either version 2 of the License, or (at
;your option) any later version.
;
;MIT/GNU Scheme is distributed in the hope that it will be useful, but
;WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;along with MIT/GNU Scheme; if not, write to the Free Software
;Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
;USA.
;
;|#

;;; Given a list of distinct abscissas xs = (x1 x2 ... xn) and a list
;;; of ordinates ys = (y1 y2 ... yn), return the Lagrange interpolation
;;; polynomial through the points (x1, y1), (x2, y2), ... (xn, yn).

(eval-when (load compile eval)
	   (set-current-module generic-environment))

(define (lagrange-interpolation-function ys xs)
  (assert (fix:= (length xs) (length ys)))
  (let ((n (length ys)))
    (define (poly x)
      (reduce + :zero
	      (generate-list n
		(lambda (i)
		  (/ (reduce * :one
		       (generate-list n
		         (lambda (j)
			   (if (fix:= j i)
			       (list-ref ys i)
			       (- x (list-ref xs j))))))
		     (let ((xi (list-ref xs i)))
		       (reduce * :one
			 (generate-list n
		           (lambda (j)
			     (cond ((fix:< j i) (- (list-ref xs j) xi))
				   ((fix:= j i) (expt :-one i))
				   (else    (- xi (list-ref xs j)))))))))))))
    poly))

;#|
;(print-expression
; ((lagrange-interpolation-function '(y1 y2 y3 y4) '(x1 x2 x3 x4)) 'x1))
;y1
;
;(print-expression
; ((lagrange-interpolation-function '(y1 y2 y3 y4) '(x1 x2 x3 x4)) 'x2))
;y2
;
;(print-expression
; ((lagrange-interpolation-function '(y1 y2 y3 y4) '(x1 x2 x3 x4)) 'x3))
;y3
;
;(print-expression
; ((lagrange-interpolation-function '(y1 y2 y3 y4) '(x1 x2 x3 x4)) 'x4))
;y4
;|#

;#|
;(pp (text/cselim
;     (expression
;      ((lagrange-interpolation-function '(y1 y2 y3 y4) '(x1 x2 x3 x4))
;       'x))))
;(let ((V-609 (- x3 x4)) (V-607 (- x2 x3)) (V-606 (* (- x2 x4) -1))
;      (V-605 (- x x1)) (V-604 (- x1 x4)) (V-603 (- x1 x3))
;      (V-602 (- x1 x2)) (V-601 (- x x2)) (V-599 (- x x3))
;      (V-598 (- x x4)))
;  (let ((V-608 (* V-601 V-605)) (V-600 (* V-598 V-599)))
;    (+ (/ (* V-600 V-601 y1) (* V-602 V-603 V-604))
;       (/ (* V-600 y2 V-605) (* V-606 V-607 V-602))
;       (/ (* V-608 V-598 y3) (* V-603 V-607 V-609))
;       (/ (* V-608 y4 V-599) (* V-606 V-609 V-604)))))
;|#












		       

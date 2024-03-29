;#| -*-Scheme-*-
;
;$Id: copyright.scm,v 1.4 2005/12/13 06:41:00 cph Exp $
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


(define (canonical? C H Hprime)
  (- (compose (phase-space-derivative H) C)
     (* (D C) (phase-space-derivative Hprime))))

(define (compositional-canonical? C H)
  (canonical? C H (compose H C)))

;#|
;(simplify
; ((compositional-canonical? (F->CT p->r)
;			    (H-central 'm (literal-function 'V)))
;  (->H-state 't
;	     (coordinate-tuple 'r 'phi)
;	     (momentum-tuple 'p_r 'p_phi))))
;;Value: (up 0 (up 0 0) (down 0 0))
;|#


(define (J-func DH)
  (->H-state 0
	     (ref DH 2)
	     (- (ref DH 1))))

(define (T-func DH)
  (->H-state 1 
	     (zero-like (ref DH 2))
	     (zero-like (ref DH 1))))

(define (J*-func s)
  (down 0
	(ref s 2)
	(- (ref s 1))))


;#|
;(print-expression
; ((D J-func)
;  (->H-state 't
;	     (coordinate-tuple 'x 'y)
;	     (momentum-tuple 'p_x 'p_y))))
;(down (up 0 (down 0 0) (up 0 0))
;      (down (up 0 (down 0 0) (up -1 0)) (up 0 (down 0 0) (up 0 -1)))
;      (up (up 0 (down 1 0) (up 0 0)) (up 0 (down 0 1) (up 0 0))))    
;|#

(define (linear-function->multiplier F argument)
  ((D F) argument))

(define ((Phi A) v) (* A v))

(define ((Phi* A) w) (* w A))


(define ((time-independent-canonical? C) s)
  ((- J-func
      (compose (Phi ((D C) s)) 
               J-func
               (Phi* ((D C) s))))
   (compatible-shape s)))

;#|
;(print-expression
; ((time-independent-canonical? (F->CT p->r))
;  (->H-state 't
;	     (coordinate-tuple 'r 'phi)
;	     (momentum-tuple 'p_r 'p_phi))))
;(up 0 (up 0 0) (down 0 0))
;
;
;;;; but not all transforms are
;
;(define (a-non-canonical-transform Istate)
;  (let ((t (time Istate))
;        (theta (coordinate Istate))
;	(p (momentum Istate)))
;    (let ((x (* p (sin theta)))
;	  (p_x (* p (cos theta))))
;      (->H-state t x p_x))))
;
;(print-expression
; ((time-independent-canonical? a-non-canonical-transform)
;  (->H-state 't 'theta 'p)))
;(up 0 (+ (* -1 p x8102) x8102) (+ (* p x8101) (* -1 x8101)))
;|#

;;; One particularly useful canonical transform is the 
;;;  Poincare transform, which is good for simplifying 
;;;  oscillators.

(define ((polar-canonical alpha) Istate)
  (let ((t (time Istate))
        (theta (coordinate Istate))
        (I (momentum Istate)))
    (let ((x (* (sqrt (/ (* 2 I) alpha)) (sin theta)))
	  (p_x (* (sqrt (* 2 alpha I)) (cos theta))))
      (->H-state t x p_x))))

(define ((polar-canonical-inverse alpha) s)
  (let ((t (time s))
	(x (coordinate s))
	(p (momentum s)))
    (let ((I (/ (+ (* alpha (square x))
		   (/ (square p) alpha))
		2)))
      (let ((theta (atan (/ x (sqrt (/ (* 2 I) alpha)))
			 (/ p (sqrt (* 2 I alpha))))))
	(->H-state t theta I)))))

;#|
;(pe
; ((compose (polar-canonical-inverse 'alpha)
;	   (polar-canonical 'alpha))
;  (->H-state 't 'x 'p)))
;(up t x p)
;
;(print-expression
; ((time-independent-canonical? (polar-canonical 'alpha))
;  (->H-state 't 'a 'I)))
;(up 0 0 0)
;|#

;#|
;(define (Cmix H-state)
;  (let ((t (time H-state))
;	(q (coordinate H-state))
;	(p (momentum H-state)))
;    (->H-state t
;	       (coordinate-tuple (ref q 0) (- (ref p 1)))
;	       (momentum-tuple   (ref p 0) (ref q 1)))))
;
;(define a-state (->H-state 't 
;			   (coordinate-tuple 'x 'y)
;			   (momentum-tuple 'p_x 'p_y)))
;
;(print-expression
; ((time-independent-canonical? Cmix)
;  a-state))
;(up 0 (up 0 0) (down 0 0))
;
;(define (Cmix2 H-state)
;  (let ((t (time H-state))
;	(q (coordinate H-state))
;	(p (momentum H-state)))
;    (->H-state t
;	       (flip-outer-index p)
;	       (- (flip-outer-index q)))))
;
;(print-expression
; ((time-independent-canonical? Cmix2)
;  a-state))
;(up 0 (up 0 0) (down 0 0))
;|#

(define ((two-particle-center-of-mass m0 m1) H-state)
  (let ((q (coordinate H-state)))
    (let ((x0 (ref q 0))
	  (x1 (ref q 1)))
      (coordinate-tuple (/ (+ (* m0 x0) (* m1 x1)) (+ m0 m1))
			(- x1 x0)))))

(define ((two-particle-center-of-mass-canonical m0 m1) state)
  (let ((x (coordinate state))
	(p (momentum state)))
    (let ((x0 (ref x 0))
	  (x1 (ref x 1))
	  (p0 (ref p 0))
	  (p1 (ref p 1)))
      (->H-state (time state)
		 (coordinate-tuple
		  (/ (+ (* m0 x0) (* m1 x1)) (+ m0 m1))
		  (- x1 x0))
		 (momentum-tuple
		  (+ p0 p1)
		  (/ (- (* m0 p1) (* m1 p0))
		     (+ m0 m1)))))))
;#|
;(define b-state
;  (->H-state 't
;	     (coordinate-tuple
;	      (coordinate-tuple 'x_1 'y_1)
;	      (coordinate-tuple 'x_2 'y_2))
;	     (momentum-tuple
;	      (momentum-tuple 'p_x_1 'p_y_1)
;	      (momentum-tuple 'p_x_2 'p_y_2))))
;
;(pe (- ((F->CT (two-particle-center-of-mass 'm0 'm1)) b-state)
;       ((two-particle-center-of-mass-canonical 'm0 'm1) b-state)))
;(up 0 (up (up 0 0) (up 0 0)) (down (down 0 0) (down 0 0)))
;
;(print-expression
; ((time-independent-canonical?
;   (two-particle-center-of-mass-canonical 'm1 'm2))
;  b-state))
;(up 0 (up (up 0 0) (up 0 0)) (down (down 0 0) (down 0 0)))
;|#

(define ((multiplicative-transpose s) A)
  (linear-function->multiplier (transpose-function A) s))

(define ((transpose-function A) p) (* p A))

;#|
;(define (T v)
;  (* (down (up 'a 'c) (up 'b 'd)) v))
;
;(pe (T (up 'x 'y)))
;(up (+ (* a x) (* b y)) (+ (* c x) (* d y)))
;
;(pe (* (* (down 'p_x 'p_y) ((D T) (up 'x 'y))) (up 'v_x 'v_y)))
;(+ (* a p_x v_x) (* b p_x v_y) (* c p_y v_x) (* d p_y v_y))
;
;
;(pe (* (down 'p_x 'p_y) (* ((D T) (up 'x 'y)) (up 'v_x 'v_y))))
;(+ (* a p_x v_x) (* b p_x v_y) (* c p_y v_x) (* d p_y v_y))
;
;(pe (* (* ((multiplicative-transpose (down 'p_x 'p_y)) ((D T) (up 'x 'y)))
;	  (down 'p_x 'p_y))
;       (up 'v_x 'v_y)))
;(+ (* a p_x v_x) (* b p_x v_y) (* c p_y v_x) (* d p_y v_y))
;
;;;; But strangely enough...
;(pe (* (* (down 'p_x 'p_y)
;	  ((multiplicative-transpose (down 'p_x 'p_y)) ((D T) (up 'x 'y))))
;       (up 'v_x 'v_y)))
;(+ (* a p_x v_x) (* b p_x v_y) (* c p_y v_x) (* d p_y v_y))
;|#

;#|
;(define ((time-independent-canonical? C) s)
;  (let ((s* (compatible-shape s)))
;    (let ((J (linear-function->multiplier J-func s*)))
;      (- J 
;	 (* ((D C) s)
;	    (* J
;	       ((multiplicative-transpose s*) ((D C) s))))))))
;
;(print-expression
; ((time-independent-canonical? (F->CT p->r))
;  (->H-state 't
;	     (coordinate-tuple 'r 'phi)
;	     (momentum-tuple 'p_r 'p_phi))))
;(up (up 0 (up 0 0) (down 0 0))
;    (up (up 0 (up 0 0) (down 0 0)) (up 0 (up 0 0) (down 0 0)))
;    (down (up 0 (up 0 0) (down 0 0)) (up 0 (up 0 0) (down 0 0))))
;
;
;;;; but not all transforms are
;
;(define (a-non-canonical-transform Istate)
;  (let ((t (time Istate))
;        (theta (coordinate Istate))
;	(p (momentum Istate)))
;    (let ((x (* p (sin theta)))
;	  (p_x (* p (cos theta))))
;      (->H-state t x p_x))))
;
;(print-expression
; ((time-independent-canonical? a-non-canonical-transform)
;  (->H-state 't 'theta 'p)))
;(up (up 0 0 0) (up 0 0 (+ -1 p)) (up 0 (+ 1 (* -1 p)) 0))
;
;(print-expression
; ((time-independent-canonical? (polar-canonical 'alpha))
;  (->H-state 't 'a 'I)))
;(up (up 0 0 0) (up 0 0 0) (up 0 0 0))
;|#

;#|
;(define (Cmix H-state)
;  (let ((t (time H-state))
;	(q (coordinate H-state))
;	(p (momentum H-state)))
;    (->H-state t
;	       (coordinate-tuple (ref q 0) (- (ref p 1)))
;	       (momentum-tuple   (ref p 0) (ref q 1)))))
;
;(define a-state (->H-state 't 
;			   (coordinate-tuple 'x 'y)
;			   (momentum-tuple 'p_x 'p_y)))
;
;(print-expression
; ((time-independent-canonical? Cmix)
;  a-state))
;(up (up 0 (up 0 0) (down 0 0))
;    (up (up 0 (up 0 0) (down 0 0)) (up 0 (up 0 0) (down 0 0)))
;    (down (up 0 (up 0 0) (down 0 0)) (up 0 (up 0 0) (down 0 0))))
;
;(define (Cmix2 H-state)
;  (let ((t (time H-state))
;	(q (coordinate H-state))
;	(p (momentum H-state)))
;    (->H-state t
;	       (flip-outer-index p)
;	       (- (flip-outer-index q)))))
;
;(print-expression
; ((time-independent-canonical? Cmix2)
;  a-state))
;(up (up 0 (up 0 0) (down 0 0))
;    (up (up 0 (up 0 0) (down 0 0)) (up 0 (up 0 0) (down 0 0)))
;    (down (up 0 (up 0 0) (down 0 0)) (up 0 (up 0 0) (down 0 0))))
;|#

;#|
;(define ((C m0 m1) state)
;  (let ((x (coordinate state))
;	(p (momentum state)))
;    (let ((x0 (ref x 0))
;	  (x1 (ref x 1))
;	  (p0 (ref p 0))
;	  (p1 (ref p 1)))
;      (->H-state 
;       (time state)
;       (coordinate-tuple (/ (+ (* m0 x0) (* m1 x1)) (+ m0 m1))
;			 (- x1 x0))
;       (momentum-tuple (+ p0 p1)
;		       (/ (- (* m0 p1) (* m1 p0))
;			  (+ m0 m1)))))))
;
;(define b-state
;  (->H-state
;   't
;   (coordinate-tuple
;    (coordinate-tuple 'x_1 'y_1)
;    (coordinate-tuple 'x_2 'y_2))
;   (momentum-tuple
;    (momentum-tuple 'p_x_1 'p_y_1)
;    (momentum-tuple 'p_x_2 'p_y_2))))
;
;(print-expression
; ((time-independent-canonical? (C 'm1 'm2)) b-state))
;(up
; (up 0 (up (up 0 0) (up 0 0)) (down (down 0 0) (down 0 0)))
; (up
;  (up (up 0 (up (up 0 0) (up 0 0)) (down (down 0 0) (down 0 0)))
;      (up 0 (up (up 0 0) (up 0 0)) (down (down 0 0) (down 0 0))))
;  (up (up 0 (up (up 0 0) (up 0 0)) (down (down 0 0) (down 0 0)))
;      (up 0 (up (up 0 0) (up 0 0)) (down (down 0 0) (down 0 0)))))
; (down
;  (down (up 0 (up (up 0 0) (up 0 0)) (down (down 0 0) (down 0 0)))
;        (up 0 (up (up 0 0) (up 0 0)) (down (down 0 0) (down 0 0))))
;  (down (up 0 (up (up 0 0) (up 0 0)) (down (down 0 0) (down 0 0)))
;        (up 0 (up (up 0 0) (up 0 0)) (down (down 0 0) (down 0 0))))))
;
;|#

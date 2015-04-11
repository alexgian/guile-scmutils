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

;;;; Interface system for QUADRATURE 

;

(define :-infinity ':-infinity)
(define :+infinity ':+infinity)
(define *infinities* (list :-infinity :+infinity))

;#| 
;;;; Bugs:
;
;;;; The following should be pi.
;;;; Unfortunately it converges so slowly that
;;;; we never get a  reasonable answer.  
;;;; The functions that we can integrate must 
;;;; decay at least as fast as 1/x^2 
;;;;  as x->infinity.
;
;(* 2
;   ((make-definite-integrator
;     (lambda->numerical-procedure
;      '(lambda (x) (/ (sin x) x)))
;     0.0
;     :+infinity
;     .01)
;    'integral))
;|#

;#|
;(define witch
;  (lambda->numerical-procedure
;   '(lambda (x)
;      (/ 4.0 (+ 1.0 (* x x))))))
;
;(define integrator (make-definite-integrator))
;
;(integrator 'set-method! 'romberg)
;(integrator 'set-error! 1e-12)
;(integrator 'set-integrand! witch)
;(integrator 'set-lower-limit! 0.0)
;(integrator 'set-upper-limit! 1.0)
;(integrator 'integral)
;;Value: 3.141592653589793
;
;;;; Easy as pi.
;|#

;#|
;(define (foo n)
;  (define int
;    (make-definite-integrator
;     (lambda (x) (expt (log (/ 1 x)) n))
;     0.0
;     1.0
;     1e-12))
;  (int 'set-method! 'open-closed)
;  (int 'integral))
;
;(foo 0)
;;Value: 1.
;
;(foo 1)
;;Value: .9999999999979357
;
;(foo 2)
;;Value: 1.9999999999979101
;
;(foo 3)
;;Value: 5.99999999999799
;
;(foo 4)
;;Value: 23.999999999997893
;
;(foo 5)
;;Value: 119.99999999999828
;
;;;; Do you recognize the function FOO?
;
;
;(define (bar)
;  (define int
;    (make-definite-integrator
;     (lambda (x) (* (exp (- x)) (log x)))
;     0.0
;     :+infinity
;     1e-11))
;  (int 'set-method! 'open-open)
;  (int 'integral))
;;Value: bar
;
;(bar)
;;Value: -.5772156648993277
;
;;;; Do you recognize this constant?
;|#

(define* (make-definite-integrator
	 #:optional integrand lower-limit upper-limit allowable-error method)
  (let* ((integrand
	  (if (default-object? integrand) #f integrand))
	 (lower-limit
	  (if (default-object? lower-limit) #f lower-limit))
	 (upper-limit
	  (if (default-object? upper-limit) #f upper-limit))
	 (allowable-error
	  (if (default-object? allowable-error) 1.0e-10 allowable-error))
	 (method
	  (if (default-object? method) 'open method)))


    (define (the-integrator-control message . rest-of-arguments)

      (case message
	
	((integral)
	 (evaluate-definite-integral
	  method
	  integrand
	  lower-limit
	  upper-limit
	  allowable-error))	  
	
	((integrand) integrand)
	((set-integrand!)
	 (let ((x (car rest-of-arguments)))
	   (set! integrand x)))

	((lower-limit) lower-limit)
	((set-lower-limit!)
	 (let ((x (car rest-of-arguments)))
	   (set! lower-limit x)))

	((upper-limit) upper-limit)
	((set-upper-limit!)
	 (let ((x (car rest-of-arguments)))
	   (set! upper-limit x)))

	((error) allowable-error)
	((set-error!)
	 (let ((x (car rest-of-arguments)))
	   (set! allowable-error x)))
	
	((method) method)
	((set-method!)
	 (let ((x (car rest-of-arguments)))
	   (set! method x)))

	(else
	 (error "Unknown message -- ODE-INTEGRATOR") message)
	))

    the-integrator-control))

(define (evaluate-definite-integral method
				    integrand
				    lower-limit
				    upper-limit
				    allowable-error)
  (if (not (and integrand lower-limit upper-limit))
      (error "Missing parameter for definite integral"
	     `(integrand ,integrand
	       lower-limit ,lower-limit
	       upper-limit ,upper-limit)))

  (let ((lower-limit (if (memq lower-limit *infinities*)
			 lower-limit
			 (exact->inexact lower-limit)))
	(upper-limit (if (memq upper-limit *infinities*)
			 upper-limit
			 (exact->inexact upper-limit)))
	(allowable-error (exact->inexact allowable-error)))
    
    (if (or (memq lower-limit *infinities*)
	    (memq upper-limit *infinities*))
	(evaluate-improper-integral method
				    integrand
				    upper-limit
				    lower-limit
				    allowable-error)
	  
	(case method

	  ((open)
	   (integrate-open integrand
			   lower-limit upper-limit
			   allowable-error))

	  ((closed-closed)
	   (integrate-closed-closed-1 integrand
				      lower-limit upper-limit
				      allowable-error))

	  ((closed-open)
	   (integrate-closed-open-1 integrand
				    lower-limit upper-limit
				    allowable-error))

	  ((open-closed)
	   (integrate-open-closed-1 integrand
				    lower-limit upper-limit
				    allowable-error))

	  ((open-open)
	   (integrate-open-open integrand
				lower-limit upper-limit
				allowable-error))

	  ((romberg)
	   (romberg-quadrature integrand
			       lower-limit upper-limit
			       allowable-error))

	  ((bulirsch-stoer)
	   (bulirsch-stoer-quadrature integrand
				      lower-limit upper-limit
				      allowable-error))
	  (else
	   (error "Unknown method -- DEFINITE-INTEGRAL" method))))))


;#|
;(define (evaluate-improper-integral method integrand upper-limit lower-limit allowable-error)
;  (let ((new-integrand
;	 (lambda (theta)
;	   (/ (integrand (tan theta))
;	      (square (cos theta))))))
;	
;    (case lower-limit
;      ((:-infinity)
;       (case upper-limit
;	 ((:+infinity)
;	  (integrate-open-open new-integrand
;			       :-pi/2 :+pi/2
;			       allowable-error))
;	 ((:-infinity) 0.0)
;	 (else
;	  (if (memq method '(open-open closed-open))
;	      (integrate-open-open new-integrand
;				   :-pi/2 (atan upper-limit)
;				   allowable-error)
;	      (integrate-open-closed new-integrand
;				     :-pi/2 (atan upper-limit)
;				     allowable-error)))))
;      ((:+infinity)
;       (case upper-limit
;	 ((:+infinity) 0.0)
;	 ((:-infinity)
;	  (- (integrate-open-open new-integrand
;				  :-pi/2 :+pi/2
;				  allowable-error)))
;	 (else
;	  (if (memq method '(open-open open-closed))
;	      (- (integrate-open-open new-integrand
;				      (atan upper-limit) :+pi/2
;				      allowable-error))
;	      (- (integrate-closed-open new-integrand
;					(atan upper-limit) :+pi/2
;					allowable-error))))))
;      (else
;       (case upper-limit
;	 ((:+infinity)
;	  (if (memq method '(open-open open-closed))
;	      (integrate-open-open new-integrand
;				   (atan lower-limit) :+pi/2
;				   allowable-error)
;	      (integrate-closed-open new-integrand
;				     (atan lower-limit) :+pi/2
;				     allowable-error)))
;	 ((:-infinity)
;	  (if (memq method '(open-open open-closed))
;	      (- (integrate-open-open new-integrand
;				      :-pi/2 (atan lower-limit)
;				      allowable-error))
;	      (- (integrate-closed-open new-integrand
;					:-pi/2 (atan lower-limit)
;					allowable-error)))))))))
;|#

;;; Simpler version, from Press, et al.

(define *improper-integral-breakpoint* +1.0)

(define (evaluate-improper-integral method integrand upper-limit lower-limit allowable-error)
  (let ((new-integrand
	 (lambda (t)
	   (/ (integrand (/ 1.0 t))
	      (* t t)))))
	
    (case lower-limit
      ((:-infinity)
       (case upper-limit
	 ((:-infinity) 0.0)
	 ((:+infinity)
	  (+ (integrate-closed-open new-integrand
				    (/ -1.0 *improper-integral-breakpoint*)
				    0.0
				    allowable-error)
	     (integrate-closed-closed integrand
				      (- *improper-integral-breakpoint*)
				      *improper-integral-breakpoint*
				      allowable-error)
	     (integrate-open-closed new-integrand
				    0.0
				    (/ 1.0 *improper-integral-breakpoint*)
				    allowable-error)))
	 (else
	  (if (<= upper-limit (- *improper-integral-breakpoint*))
	      (integrate-open-open new-integrand
				   (/ -1.0 upper-limit)
				   0.0
				   allowable-error)
	      (+ (integrate-closed-open new-integrand
					(/ -1.0 *improper-integral-breakpoint*)
					0.0
					allowable-error)
		 (integrate-closed-open integrand
					(- *improper-integral-breakpoint*)
					upper-limit
					allowable-error))))))
      ((:+infinity)
       (case upper-limit
	 ((:-infinity)
	  (- (+ (integrate-closed-open new-integrand
				       (/ -1.0 *improper-integral-breakpoint*)
				       0.0
				       allowable-error)
		(integrate-closed-closed integrand
					 (- *improper-integral-breakpoint*)
					 *improper-integral-breakpoint*
					 allowable-error)
		(integrate-open-closed new-integrand
				       0.0
				       (/ 1.0 *improper-integral-breakpoint*)
				       allowable-error))))
	 ((:+infinity) 0.0)
	 (else
	  (if (>= upper-limit *improper-integral-breakpoint*)
	      (- (integrate-open-open new-integrand
				      (/ 1.0 upper-limit)
				      0.0
				      allowable-error))
	      (- (+ (integrate-closed-open new-integrand
					   (/ 1.0 *improper-integral-breakpoint*)
					   0.0
					   allowable-error)
		    (integrate-closed-open integrand
					   *improper-integral-breakpoint*
					   upper-limit
					   allowable-error)))))))
      (else
       (case upper-limit
	 ((:-infinity)
	  (if (<= lower-limit (- *improper-integral-breakpoint*))
	      (integrate-open-open new-integrand
				   (/ -1.0 lower-limit)
				   0.0
				   allowable-error)
	      (+ (integrate-closed-open new-integrand
					(/ -1.0 *improper-integral-breakpoint*)
					0.0
					allowable-error)
		 (integrate-closed-open integrand
					*improper-integral-breakpoint*
					lower-limit
					allowable-error))))
	 ((:+infinity)
	  (if (>= lower-limit *improper-integral-breakpoint*)
	      (integrate-open-open new-integrand
				   0.0
				   (/ 1.0 lower-limit)
				   allowable-error)
	      (+ (integrate-open-closed integrand
					lower-limit
					*improper-integral-breakpoint*
					allowable-error)
		 (integrate-open-closed new-integrand
					0.0
					(/ 1.0 *improper-integral-breakpoint*)
					allowable-error))))
	 (else
	  (error "Should not get here -- IMPROPER-INTEGRAL")))))))

(define (bulirsch-stoer-quadrature f t1 t2 allowable-error)
  ((advance-generator
    (bulirsch-stoer-lisptran
     ;; state = #(t int) ==> dstate = #(1.0 ,(integral f t1 t))
     (lambda (state dstate)
       (vector-set! dstate 0 1.0)
       (vector-set! dstate 1
		    (f (vector-ref state 0))))
     2
     allowable-error))
   (vector t1 0.0)			;initial state
   (- t2 t1)
   (/ (- t2 t1) 2)
   (- t2 t1)
   (lambda (ns dt h cont)
     (cont))
   (lambda (ns dt sdt)
     (vector-ref ns 1))))

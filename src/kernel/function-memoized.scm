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

;;;;            Functions

(declare (usual-integrations))

(define (f:type f) function-type-tag)
(define (f:type-predicate f) function-quantity?)

;;; Arity manipulation procedures are in UTILS.SCM

(define ((f:unary operator) f)
  (compose-bin operator f))

(define ((f:binary operator) f1 f2)
  (let ((a (joint-arity (g:arity f1) (g:arity f2))))
    (if (not a)
	(error "Functions have different arities" f1 f2))
    (let ((f1 (if (function? f1) f1 (coerce-to-function f1)))
	  (f2 (if (function? f2) f2 (coerce-to-function f2))))	
      (cond ((equal? a *at-least-zero*)
	     (lambda x
	       (operator (apply f1 x) (apply f2 x))))
	    ((equal? a *exactly-zero*)
	     (lambda ()
	       (operator (f1) (f2))))
	    ((equal? a *at-least-one*)
	     (lambda (x . y)
	       (operator (apply f1 x y) (apply f2 x y))))
	    ((equal? a *exactly-one*)
	     (lambda (x)
	       (operator (f1 x) (f2 x))))
	    ((equal? a *at-least-two*)
	     (lambda (x y . z)
	       (operator (apply f1 x y z) (apply f2 x y z))))
	    ((equal? a *exactly-two*)
	     (lambda (x y)
	       (operator (f1 x y) (f2 x y))))
	    ((equal? a *at-least-three*)
	     (lambda (u x y . z)
	       (operator (apply f1 u x y z) (apply f2 u x y z))))
	    ((equal? a *exactly-three*)
	     (lambda (x y z)
	       (operator (f1 x y z) (f2 x y z))))
	    ((equal? a *one-or-two*)
	     (lambda (x #!optional y)
	       (if (default-object? y)
		   (operator (f1 x) (f2 x))
		   (operator (f1 x y) (f2 x y)))))
	    (else
	     (lambda x
	       (operator (apply f1 x) (apply f2 x))))))))

(define ((coerce-to-function g) . x)
  (if (numerical-quantity? g)
      g
      (g:apply g x)))

(define (f:arity f) (procedure-arity f))

(define (f:zero-like f)			;want (zero-like range-element)
  (lambda any (g:zero-like (apply f any))))

(define (f:one-like f)			;want (one-like range-element)
  (lambda any (g:one-like (apply f any))))

(define (f:identity-like f) g:identity)

(assign-operation 'type            f:type            function?)
(assign-operation 'type-predicate  f:type-predicate  function?)
(assign-operation 'arity           f:arity           function?)

(assign-operation 'inexact?        (f:unary g:inexact?)                 function?)

(assign-operation 'zero-like       (linear-memoize-1arg f:zero-like)    function?)
(assign-operation 'one-like        f:one-like                 function?)
(assign-operation 'identity-like   f:identity-like            function?)

;;; The following tests conflict with the conservative theory of
;;; generic predicates in that they return a new procedure with a
;;; deferred test rather than #f, because they cannot know the
;;; result.  Indeed, a user may write (compose zero? f) if necessary.

;;;(assign-operation 'zero?           (f:unary g:zero?)          function?)
;;;(assign-operation 'one?            (f:unary g:one?)           function?)
;;;(assign-operation 'identity?       (f:unary g:identity?)      function?)

(assign-operation 'negate   (linear-memoize-1arg (f:unary g:negate))     function?)
(assign-operation 'invert   (linear-memoize-1arg (f:unary g:invert))     function?)

(assign-operation 'sqrt     (linear-memoize-1arg (f:unary g:sqrt))       function?)
(assign-operation 'square   (linear-memoize-1arg (f:unary g:square))     function?)

(assign-operation 'exp      (linear-memoize-1arg (f:unary g:exp))        function?)
(assign-operation 'log      (linear-memoize-1arg (f:unary g:log))        function?)

(assign-operation 'sin      (linear-memoize-1arg (f:unary g:sin))        function?)
(assign-operation 'cos      (linear-memoize-1arg (f:unary g:cos))        function?)

(assign-operation 'asin     (linear-memoize-1arg (f:unary g:asin))       function?)
(assign-operation 'acos     (linear-memoize-1arg (f:unary g:acos))       function?)

(assign-operation 'sinh     (linear-memoize-1arg (f:unary g:sinh))       function?)
(assign-operation 'cosh     (linear-memoize-1arg (f:unary g:cosh))       function?)

(assign-operation 'abs      (linear-memoize-1arg (f:unary g:abs))        function?)

;;; Binary operations on functions are a bit weird.  A special predicate
;;; are needed to make the correct coercions possible:

;;; Tests must be conservative.
;;;(assign-operation '=               (f:binary g:=)             function? function?)

(assign-operation '+          (linear-memoize (f:binary g:+))    function? cofunction?)
(assign-operation '+          (linear-memoize (f:binary g:+))    cofunction? function?)
(assign-operation '-          (linear-memoize (f:binary g:-))    function? cofunction?)
(assign-operation '-          (linear-memoize (f:binary g:-))    cofunction? function?)
(assign-operation '*          (linear-memoize (f:binary g:*))    function? cofunction?)
(assign-operation '*          (linear-memoize (f:binary g:*))    cofunction? function?)
(assign-operation '/          (linear-memoize (f:binary g:/))    function? cofunction?)
(assign-operation '/          (linear-memoize (f:binary g:/))    cofunction? function?)

(assign-operation 'expt       (linear-memoize (f:binary g:expt)) function? cofunction?)
(assign-operation 'expt       (linear-memoize (f:binary g:expt)) cofunction? function?)

(assign-operation 'gcd        (linear-memoize (f:binary g:gcd))  function? cofunction?)
(assign-operation 'gcd        (linear-memoize (f:binary g:gcd))  cofunction? function?)

(assign-operation 'make-rectangular
		  (linear-memoize (f:binary g:make-rectangular))
		  function? cofunction?)
(assign-operation 'make-rectangular
		  (linear-memoize (f:binary g:make-rectangular))
		  cofunction? function?)

(assign-operation 'make-polar
		  (linear-memoize (f:binary g:make-polar))
		  function? cofunction?)
(assign-operation 'make-polar
		  (linear-memoize (f:binary g:make-polar))
		  cofunction? function?)

(assign-operation 'real-part  (linear-memoize-1arg (f:unary g:real-part))      function?)
(assign-operation 'imag-part  (linear-memoize-1arg (f:unary g:imag-part))      function?)
(assign-operation 'magnitude  (linear-memoize-1arg (f:unary g:magnitude))      function?)
(assign-operation 'angle      (linear-memoize-1arg (f:unary g:angle))          function?)

(assign-operation 'conjugate  (linear-memoize-1arg (f:unary g:conjugate))      function?)

(assign-operation 'atan1      (linear-memoize-1arg (f:unary g:atan))           function?)
(assign-operation 'atan2      (linear-memoize (f:binary g:atan)) function? cofunction?)
(assign-operation 'atan2      (linear-memoize (f:binary g:atan)) cofunction? function?)

;;; PARTIAL-DERIVATIVE and APPLY are special for functions.
;;; (assign-operation 'partial-derivative f:partial-derivative  function?  any?)
;;; (assign-operation 'apply              f:apply               function?  any?)

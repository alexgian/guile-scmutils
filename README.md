# guile-scmutils

**This repository is intended for my own personal use and learning about the `scmutils` system, and the software in it may or may not work at any given time. If you need `scmutils` for a class or for your own interest, do not use this! Instead, please use [Daniel Gildea's version of `scmutils` for Guile](http://www.cs.rochester.edu/~gildea/guile-scmutils/), or better still [the original version for MIT Scheme](http://www-swiss.ai.mit.edu/~gjs/6946/linux-install.htm) if you have the wherewithal to install it.**

This is a port of the [`scmutils`](http://www-swiss.ai.mit.edu/~gjs/6946/linux-install.htm) package for symbolic mathematics from [MIT Scheme](http://www.gnu.org/software/mit-scheme/) to [Guile](http://www.gnu.org/software/guile/guile.html), based on version 1.0 of the [port](http://www.cs.rochester.edu/~gildea/guile-scmutils/) by [Daniel Gildea](http://www.cs.rochester.edu/~gildea/).

`scmutils` is required to run the examples from the book [Structure and Interpretation of Classical Mechanics](http://mitpress.mit.edu/SICM/).

## Requirements

Requires guile 2.0 or higher.

Plotting is implemented through calls to [gnuplot](http://www.gnuplot.info), which must be installed separately.

## Usage

To use the package, it must be on guile's load path. One simple way to do this is to run
```Scheme
(add-to-load-path "/path/to/guile-scmutils")
```
After this has been done, the `scmutils` module can be loaded by running
```Scheme
(use-modules (scmutils))
```

## Functionality not available in the port

Guile does not have the MIT Scheme extension to allow applying vectors/structures as procedures.

For example rather than
```Scheme
scheme@(guile-user)> (pe ((up (literal-function 'x) (literal-function 'y)) 't))
(up (x t) (y t))
```
you must use
```Scheme
scheme@(guile-user)> (pe ((lambda (t) (up ((literal-function 'x) t) ((literal-function 'y) t))) 't))
(up (x t) (y t))
```
See below for an example with mechanics state functions.

## Example session

<pre>
% guile
guile> (load "load.scm")
guile> (set-current-module generic-environment)
guile> (define D derivative)
guile> (define f (literal-function 'f))
guile> (define f^2 (expt f 2))
guile> (pe ((D f^2) 't))
<strong>(* ((derivative f) t) 2 (f t))</strong>

guile> (pe ((D sin) 's))
<strong>(cos s)</strong>

guile> (pe ((partial-derivative (lambda (x y) (* x y)) 0) 's 't ))
<strong>t</strong>

guile> (pe ((partial-derivative (lambda (x y) (* x y)) 1) 's 't ))
<strong>s</strong>

guile> (pp (expression
  (let ((k (literal-number 'k)) (m (literal-number 'm)))
    ((D
      (lambda (v)
       (let ((t (s:ref v 0))
             (q (s:ref v 1))
             (p (s:ref v 2)))
         (+ (/ (square p)
                   (* 2 m))
            (* 1/2 k (square q))
            (sin t)))))
     (up (literal-number 't)
        (up (literal-number 'x)
            (literal-number 'y))
        (down (literal-number 'px)
              (literal-number 'py)))))))
<strong>(down (cos t)
      (down (* 0.5 k (+ x x)) (* 0.5 k (+ y y)))
      (up (* (+ px px) (/ 1 (* 2 m)))
          (* (+ py py) (/ 1 (* 2 m)))))</strong>

; can't apply vector as function.
; must define q as a lambda expr instead of as a vector of literal functions.
guile> (define q (lambda (t) (up ((literal-function 'x) t)  
			  ((literal-function 'y) t)  
			  ((literal-function 'z) t))))

guile> (define ((L-free-particle mass) local)
  (let ((v (ref local 2)))
    (* 1/2 mass (square v))))

guile> (define ((Gamma q) t)
  (up t
      (q t)
      ((D q) t)))

guile> (define* ((Lagrange-equations Lagrangian #:optional dissipation-function) q)
  (let ((state-path (Gamma q)))
    (if (default-object? dissipation-function)
	(- (D (compose ((partial 2) Lagrangian) state-path))
	   (compose ((partial 1) Lagrangian) state-path))
	(- (D (compose ((partial 2) Lagrangian) state-path))
	   (compose ((partial 1) Lagrangian) state-path)
	   (- (compose ((partial 2) dissipation-function) state-path))))))

guile> (define (test-path t)
  (up (+ (* 'a t) 'a0)
      (+ (* 'b t) 'b0)
      (+ (* 'c t) 'c0)))

guile> (pp (expression ((Gamma q) 't)))
<strong>(up t
    (up (x t) (y t) (z t))
    (up ((derivative x) t)
        ((derivative y) t)
        ((derivative z) t)))</strong>

guile> (pp (expression (((Lagrange-equations (L-free-particle 'm)) test-path) 't)))
<strong>(down 0 0 0)</strong>
</pre>

## Copying

This software is licensed under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version. For details see the files `LICENSE` and `COPYING` included with the source distribution.

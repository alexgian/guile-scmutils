# guile-scmutils

This is a mirror of v1.0 of
[Daniel Gildea's](http://www.cs.rochester.edu/~gildea/)
[port](http://www.cs.rochester.edu/~gildea/guile-scmutils/) of the
[scmutils](http://www-swiss.ai.mit.edu/~gjs/6946/linux-install.htm)
package for symbolic mathematics from
[mit scheme](http://www.gnu.org/software/mit-scheme/) to
[guile](http://www.gnu.org/software/guile/guile.html), in an effort to
make scmutils and the examples from the
[Structure and Interpretation of Classical Mechanics](http://mitpress.mit.edu/SICM/)
available on a wider variety of architectures/operating systems.

After loading the package, call (set-current-module generic-environment)
to use the generic operators for +, \*, etc that can take
functions, vectors, etc as operands.  If you do not want to replace the
standard scheme operators, so you can call generic operators with g:+, g:\*, etc.

Also included is an emacs mode which displays 
tex-formatted output from an scmutils session inline in your
emacs buffer (based on <a href="http://members3.jcom.home.ne.jp/imaxima/">imaxima.el</a> for maxima).

Requires: guile 2.0 or higher.  
Plotting is implemented through calls to <a href="http://www.gnuplot.info">gnuplot</a>, which must
be installed separately.  

## Functionality not available in the port

<ul>
<li>Scheme extension to allow applying vectors/structures as procedures.
For example rather than
<pre>1 ]=> (pe ((up (literal-function 'x) (literal-function 'y)) 't))
(up (x t) (y t))
</pre>
you must use
<pre>guile> (pe ((lambda (t) (up ((literal-function 'x) t) ((literal-function 'y) t))) 't))
(up (x t) (y t))
</pre>
See below for an example with mechanics state functions.
</ul>

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

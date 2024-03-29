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

;;; Approach to a train of impulses by Fourier Series

(define ((delta-train-1 T n) f)
  (+ 1
     (* 2
	(sigma (lambda (k) (cos (* :2pi k f T)))
	       1 n))))

(plot-trace 1
	    (sigfun:make (delta-train-1 4/20 1)
			 (sigfun:make-span -10 10))
	    #t)
;Value: (1 (-10. 10. -1. 3.))

(plot-trace 2
	    (sigfun:make (delta-train-1 4/20 2)
			 (sigfun:make-span -10 10))
	    #t)
;Value: (2 (-10. 10. -1.249802888503238 5.))

(plot-trace 3
	    (sigfun:make (delta-train-1 4/20 3)
			 (sigfun:make-span -10 10))
	    #t)
;Value: (3 (-10. 10. -1.6305258709536838 7.))

(plot-trace 4
	    (sigfun:make (delta-train-1 4/20 4)
			 (sigfun:make-span -10 10))
	    #t)
;Value: (4 (-10. 10. -2.038860584611081 9.))

(plot-trace 5
	    (sigfun:make (delta-train-1 4/20 5)
			 (sigfun:make-span -10 10))
	    #t)
;Value: (5 (-10. 10. -2.454304586699323 11.))

(plot-trace 6
	    (sigfun:make (delta-train-1 4/20 20)
			 (sigfun:make-span -10 10))
	    #f)
;Value: (6 (-10. 10. -8.919305025438161 41.))

(plot-trace 7
	    (sigfun:make (delta-train-1 4/20 100)
			 (sigfun:make-span -10 10))
	    #f)
;Value: (7 (-10. 10. -39.75767264982659 201.))





;;; Fourier transform of the approximate train

(plot-trace 1
	    (sigfun:make (delta-train-1 4/24 127)
			 (sigfun:make-span -12 +12)))
;Value: (1 (-12. 12. -1.0000000000034985 255.))

(plot-trace 2
	    (Fourier-transform
	     (sigfun:make (delta-train-1 4/24 127)
			  (sigfun:make-span -12 +12))))
;Value: (2 (-21.333333333333332 21.333333333333332 0. 24.))

(plot-trace 3
	    (sigfun:make (delta-train-1 6/24 85)
			 (sigfun:make-span -12 +12)))
;Value: (3 (-12. 12. -35.45409270348447 171.))

(plot-trace 4
	    (Fourier-transform
	     (sigfun:make (delta-train-1 6/24 85)
			  (sigfun:make-span -12 +12))))
;Value: (4 (-21.333333333333332 21.333333333333332 0. 24.))

(plot-trace 5
	    (sigfun:make (delta-train-1 12/24 42)
			 (sigfun:make-span -12 +12)))
;Value: (5 (-12. 12. -17.480523007404784 85.))

(plot-trace 6
	    (Fourier-transform
	     (sigfun:make (delta-train-1 12/24 42)
			  (sigfun:make-span -12 +12))))
;Value: (6 (-21.333333333333332 21.333333333333332 0. 24.))

(plot-trace 7
	    (Fourier-transform
	     (sigfun:make (delta-train-1 24/24 21)
			  (sigfun:make-span -12 +12))))
;Value: (7 (-21.333333333333332 21.333333333333332 0. 24.))
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


(define (inverting-amplifier rs rf op)
  (cascade (series->2-port (resistor rs))
	   (parallel-parallel op
			      (series->2-port (resistor rf)))))

(define LF357
  (opamp 1e5				;Avol
	 10.0				;Rout
	 1e12				;Rin
	 (* :2pi 60)			;principal-pole
	 ))

;#|
;(cpp
; (rcf:simplify-and-flatten
;  ((voltage-transfer-ratio
;    (inverting-amplifier '1000 '10000 LF357))
;   's)))
;
;(/ (+ 90826.86366957377 (* 9.082652126165545e-4 s))
;   (+ -8705.661007734769 (* 1. s)))
;;Unspecified return value
;|#
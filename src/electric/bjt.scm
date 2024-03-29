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

;;;; Nonlinear Hybrid-pi model -- from Getreu p.18

(define (safe-exp x)
  (if (differential? x)
      (exp x)
      (if (< x -745) 0 (exp x))))

(define ((diode-current I0 q/kT) v)
  (* I0 (- (safe-exp (* q/kT v)) 1)))

;;; my convention: 
;;;   npn: voltages measured from collector to base to emitter
;;;        currents measured into collector and base and out of emitter. 
;;;   pnp: voltages measured from emitter to base to collector
;;;        currents measured into emitter and out of base and collector.

(define ((npn-currents IS q/kT beta-f beta-r) VBE VCE continue)
  ;; continue = (lambda (IC IB IE) ...)
  (let ((ICC ((diode-current IS q/kT) VBE))
	(IEC ((diode-current IS q/kT) (- VBE VCE))))
    (let ((ICT (- ICC IEC)))
      (let ((IC (- ICT (/ IEC beta-r)))
	    (IB (+ (/ ICC beta-f) (/ IEC beta-r)))
	    (IE (+ ICT (/ ICC beta-f))))
	(continue IC IB IE)))))

(define ((pnp-currents IS q/kT beta-f beta-r) VEB VEC continue)
  ;; continue = (lambda (IC IB IE) ...)
  (let ((ICC ((diode-current IS q/kT) VEB))
	(IEC ((diode-current IS q/kT) (- VEB VEC))))
    (let ((ICT (- ICC IEC)))
      (let ((IC (- ICT (/ IEC beta-r)))
	    (IB (+ (/ ICC beta-f) (/ IEC beta-r)))
	    (IE (+ ICT (/ ICC beta-f))))
	(continue IC IB IE)))))

;#|
;((npn-currents 1e-12 38 100 5) .55 20 list)
;;Value: (1.193313824254992e-3 1.193313803054992e-5 1.205246962285542e-3)
;
;((pnp-currents 1e-12 38 100 5) .55 20 list)
;;Value: (1.193313824254992e-3 1.193313803054992e-5 1.205246962285542e-3)
;
;((D (lambda (vbe)
;      ((npn-currents 1e-12 38 100 5)
;       vbe -20 (lambda (IC IB IE) IC))))
; .55)
;;Value: .04534592531408971
;
;
;(* 38 1.193313824254992e-3)
;;Value: .0453459253216897
;|#


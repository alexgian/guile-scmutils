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

;;;; Current stable of ODE integrators

;;; Needs GENERAL/TABLE 
(load "advance.scm" )

;;; Needs NUMERICS/LINEAR/GAUSS-JORDAN
(load "qc.scm" )

(load "bulirsch-stoer.scm" )

;;; Needs NUMERICS/LINEAR/FULL-PIVOT
(load "gear.scm" )

(load "ode-advancer.scm" )

(load "interface.scm" )


;;;; $Id$
;;;; Copyright (C) 1999, 2000 Greg J. Badros
;;;; 
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2, or (at your option)
;;;; any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU General Public License
;;;; along with this software; see the file COPYING.  If not, write to
;;;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;; Boston, MA 02111-1307 USA
;;;; 



(define-module (app scwm corba)
  :use-module (app scwm scwmcorba))




;; CORBA server gets initialized by the C module
;; GJB:FIXME:: split that out into a primitive
(define-public (publish-scwm-evaluator-servant)
  "Publish the scwm-evaluator-servant by putting its IOR as a property on the root window.
The \"SCWM_EVALUATOR_IOR\" property of the root window will contain a
printable string for Corba clients to use to access the scwm-scheme-evaluator interface."
  (X-property-set! 'root-window "SCWM_EVALUATOR_IOR" (corba-evaluator-ior)))

#lang racket

;; ----------------------------------------------------------------------------
;; The Delta programming language
;; Copyright (C) 2012 Paul Griffioen
;; 
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 2.1 of the License, or (at your option) any later version.
;; 
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;; 
;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
;; ----------------------------------------------------------------------------

(require "runtime.rkt")

(provide d_make_semaphore
         d_up
         d_down)

(define (d_make_semaphore n) = (make-semaphore n))

(define (d_up x) = (semaphore-post x) (empty-state))

(define (d_down x) = (semaphore-wait x) (empty-state))


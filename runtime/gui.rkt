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
(require racket/gui/base)

(provide d_make_window
         d_set_background
	 d_color
	 d_rgb
	 d_draw_point
         d_draw_line
	 d_draw_rectangle
	 d_draw_ellipse
	 d_refresh)

(define (d_make_window label width height paint)
  (let ([new-es (make-eventspace)])
    (parameterize ([current-eventspace new-es])
      (let ((frame (new frame% (label label) (width width) (height height))))
        (let ((canvas (new canvas% [parent frame]
                                   [paint-callback (lambda (canvas dc) (paint #f #f #f canvas dc))])))
          (send frame show #t)
          canvas)))))

(define (d_set_background canvas color)
  (send canvas set-canvas-background color)
  (empty-state))
  
(define (d_color string)
  (make-object color% string))

(define (d_rgb r g b a)
  (make-object color% (round (inexact->exact r)) (round (inexact->exact g)) (round (inexact->exact b)) a))
  
(define (d_refresh canvas)
  (send canvas refresh-now)
  (empty-state))

(define (d_draw_point dc x y thickness color)
  (send dc set-pen color thickness 'solid)
  (send dc draw-point x y)
  (empty-state))
  
(define (d_draw_line dc x y width height color)
  (send dc set-pen color 1 'solid)
  (send dc draw-line x y width height)
  (empty-state))
  
(define (d_draw_rectangle dc x y width height color)
  (send dc set-brush color 'solid)
  (send dc set-pen color 1 'transparent)
  (send dc draw-rectangle x y width height)
  (empty-state))

(define (d_draw_ellipse dc x y width height color)
  (send dc set-brush color 'solid)
  (send dc set-pen color 1 'transparent)
  (send dc draw-ellipse x y width height)
  (empty-state))

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

(provide d_open_input_file
         d_open_output_file
         d_close_file
         d_read_char
         d_peek_char
         d_eof
         d_whitespace
         d_alphabetic
         d_numeric
         d_file_position
         d_file_lines)

(define (d_file_position handle)
  (let-values (((a b c) (port-next-location handle)))
    (list a b c)))

(define (d_whitespace char)
  (if (eof-object? char) #f (char-whitespace? char)))

(define (d_alphabetic char)
  (if (eof-object? char) #f (char-alphabetic? char)))

(define (d_numeric char)
  (if (eof-object? char) #f (char-numeric? char)))

(define (d_open_input_file name)
  (let ((port (open-input-file name #:mode 'text)))
    (port-count-lines! port)
    port))

(define (d_open_output_file name)
  (open-output-file name #:mode 'text #:exists 'replace))

(define (d_close_file handle)
  (if (input-port? handle)
      (close-input-port handle)
      (close-output-port handle))
  (empty-state))

(define (d_read_char handle) (read-char handle))
  
(define (d_peek_char handle) (peek-char handle))

(define (d_eof handle) (eof-object? (peek-char handle)))

(define (d_file_lines handle)
  (port->lines handle))


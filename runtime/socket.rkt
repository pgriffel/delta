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
(require racket/tcp)

(provide d_listen_on
         d_close
         d_accept
         d_connect_to
         d_send_to
         d_receive_from)

(define (d_listen_on host port)
  (tcp-listen (string->number port) 5 #f host))

(define (d_close socket)
  (if (list? socket) 
    (begin
      (close-input-port (second socket))
      (close-output-port (third socket)))
    (tcp-close socket))
  (empty-state))

(define (d_accept socket)
  (let-values (((in out) (tcp-accept socket)))
    (list 'connection in out)))

(define (d_connect_to host port)
  (let-values (((in out) (tcp-connect host (string->number port))))
    (list 'connection in out)))

(define (d_send_to connection text)
  (if (and (list? connection) 
           (not (null? connection))
           (equal? (first connection) 'connection))
    (begin
      (write text (third connection))
      (flush-output (third connection))
      (empty-state))
    (error (format "Expected a connection but got a ~A" connection))))

(define (d_receive_from connection)
  (if (and (list? connection) 
           (not (null? connection))
           (equal? (first connection) 'connection))
    (let ((value (read (second connection))))
      (if (eof-object? value)
        (error "connection closed")
	value))
    (error (format "Expected a connection but got a ~A" connection))))


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

(provide d_open_channel
         d_send
         d_receive
         d_channel_name)

;; ----------------------------------------------------------------------------
;; Channels implementation
;; ----------------------------------------------------------------------------

(define *channel-mutex* (make-semaphore 1))

(define (d_open_channel x) =
  (let ((channel (instantiate delta-channel% ())))
    (send channel set-name! x)
    channel))

(define (d_channel_name channel) = 
  (send channel name))

(define (d_send channel message) = 
    (semaphore-wait *channel-mutex*)
    (let ((semaphore (make-semaphore 0)))
      (send channel set-senders! (fifo-append (cons message semaphore) 
                                      (send channel senders)))
      (unless (fifo-empty? (send channel receivers))
        (semaphore-post (fifo-first (send channel receivers))))
      (semaphore-post *channel-mutex*)
      (semaphore-wait semaphore))
    (empty-state))

(define (d_receive channels) =
    (unless (list? channels)
      (error (format "Function 'receive' wants a list but got a ~A" channels)))
    (semaphore-wait *channel-mutex*)
    (let ((channel (find-channel channels)))
      (let ((entry (fifo-first (send channel senders))))
        (send channel set-senders! (fifo-rest (send channel senders)))
        (semaphore-post (cdr entry))
        (for-each (lambda (x) (when (another-rendezvous? x)
                                (error "huh")
                                (semaphore-post x)))
                  channels)
        (semaphore-post *channel-mutex*)
        (merge-state (singleton-state 'l_message (car entry))
                     (singleton-state 'l_channel channel)))))

(define find-channel
  (lambda (channels)
    (let-values (((chn found) (find sender-waiting? channels)))
      (if found chn
          (let ((semaphore (make-semaphore 0)))
            (for-each (lambda (x) 
                        (send x set-receivers! (fifo-append semaphore (send x receivers))))
                      channels)
            (semaphore-post *channel-mutex*)
            (semaphore-wait semaphore)
            (semaphore-wait *channel-mutex*)
            (let-values (((chn found) (find (channel-available semaphore)
                                            channels)))
              (for-each (lambda (x)
                          (send x set-receivers! (fifo-remove semaphore
                                                         (send x receivers))))
                        channels)
              chn))))))

;; ----------------------------------------------------------------------------
;; Data Structures
;; ----------------------------------------------------------------------------

(define make-fifo-queue (lambda () '()))
(define fifo-append (lambda (entry queue) (append queue (list entry))))
(define fifo-remove remove)
(define fifo-empty? null?)
(define fifo-first car)
(define fifo-rest cdr)

(define delta-channel%
  (class object%
    
    (public senders set-senders! receivers set-receivers! name set-name!)
        
    (define senders-queue (make-fifo-queue))
    (define receivers-queue (make-fifo-queue))
    (define name-slot "unknown")
    
    (define (senders) senders-queue)
    (define (set-senders! queue) (set! senders-queue queue))
    (define (receivers) receivers-queue)
    (define (set-receivers! queue) (set! receivers-queue queue))
    (define (name) name-slot)
    (define (set-name! string) (set! name-slot string))
    
    (super-instantiate ())))
  
(define (sender-waiting? x)
  (and (not (fifo-empty? (send x senders)))
       (fifo-empty? (send x receivers))))

(define (receiver-waiting? x)
  (and (fifo-empty? (send x senders))
       (not (fifo-empty? (send x receivers)))))

(define (another-rendezvous? x)
  (and (not (fifo-empty? (send x senders)))
       (not (fifo-empty? (send x receivers)))))

(define (channel-available s)
    (lambda (x)
      (and (not (fifo-empty? (send x senders)))
           (if (fifo-empty? (send x receivers)) 
               #t
               (eq? (fifo-first (send x receivers)) s)))))

(define (find pred list)
    (cond ((null? list) (values #f #f))
          ((pred (car list)) (values (car list) #t))
          (else (find pred (cdr list)))))

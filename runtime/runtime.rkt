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

(require racket/mpair)

(provide empty-state
         singleton-state
         lookup
         merge-state
         state

	 try-catch
         para
	 leave-collective
         while
         
         d_tuple
         d_apply

         d_merge

         d_equal
         d_not
         d_not_equal
         d_greater
         d_less
         
         d_sum
         d_minus
	 d_neg
         d_multiply
         d_expt
         d_exp
         d_sqrt
	 d_divide
         d_mod
         d_div
	 d_random
	 d_random_integer
	 d_round
	 d_floor
	 d_ceiling

	 d_cons
         d_empty_list
         d_first
         d_rest
         d_list_length

	 d_make_array
         d_array_from_list
         d_get
         d_set
         d_array_length
         
         d_concatenate
	 d_split_string
         d_string
         d_characters
         
         d_print
         d_print_line
         d_write
         d_write_line

         d_read_num
         d_ask_line
         d_format

	 d_now
         d_sleep
         d_throw
         d_run_shell_command
         d_home_dir)

;; ----------------------------------------------------------------------------
;; State
;; ----------------------------------------------------------------------------

(define (empty-state) = (list))

(define (singleton-state var val) = (list (cons var val)))

(define (lookup var state) = (let ((hit (assoc var state eq?)))
                               (if hit (cdr hit) (error (format "var ~A not found" var)))))
				    
(define (merge-state x y) =
  (if (null? y)
      x
      (merge-state (merge-state-aux x (first y)) (rest y))))

(define (merge-state-aux state singleton) =
  (if (null? state)
      (list singleton)
      (if (eq? (car singleton) (car (first state)))
          (cons singleton (rest state))
	  (cons (first state) (merge-state-aux (rest state) singleton)))))

(define (state . pairs)
  (if (null? pairs) 
      (empty-state)
      (cons (cons (first pairs) (second pairs))
            (apply state (rest (rest pairs))))))

;; ----------------------------------------------------------------------------
;; Utilities
;; ----------------------------------------------------------------------------

(define (try-catch body handler)
  (let ((cust (current-custodian))
        (new (make-custodian))
	(result (empty-state)))
    (current-custodian new)
    (with-handlers ((exn:fail? handler)) (set! result (body)))
    (custodian-shutdown-all new)
    (current-custodian cust)
    result))

(define-syntax (while stx)
  (syntax-case stx ()
      ((_ condition expression ...)
       #`(do ()
           ((not condition))
           expression
           ...))))

(define (para x y link)
  (if (not link)
      (let ((link (setup-collective)))
        (run-child link (lambda () (para x y link)))
        (wait-for-collective link))
      (let ((new (join-collective link)))
	(run-child link (lambda () (x new)))
        (y link))))

;; Non tail-recursive version:

;;(define (para x y w) = (let ((sx '())
;;                             (sy '())
;;			     (err1 #f)
;;			     (err2 #f)
;;                             (sem (make-semaphore 0)))
;;                         (let ((thread1 (thread (lambda ()
;;			                  (with-handlers ((exn:fail?
;;                                                           (lambda (x)
;;                                                             (set! err1 x)
;;							     (semaphore-post sem))))
;;                                            (set! sx (x #f))
;;			                    (semaphore-post sem)))))
;;	                       (thread2 (thread (lambda ()
;;			                  (with-handlers ((exn:fail?
;;                                                           (lambda (x)
;;                                                             (set! err2 x)
;;							     (semaphore-post sem))))
;;                                            (set! sy (y #f))
;;			                    (semaphore-post sem))))))
;;                           (semaphore-wait sem)
;;			   (cond (err1 (kill-thread thread2) (semaphore-post sem))
;;			         (err2 (kill-thread thread1) (semaphore-post sem)))
;;                           (semaphore-wait sem)
;;                           (cond (err1 (kill-thread thread2) (error (exn-message err1)))
;;			         (err2 (error (exn-message err2)))
;;			         (else (merge-state sx sy))))))


;; ----------------------------------------------------------------------------
;; Primitives
;; ----------------------------------------------------------------------------

(define d_tuple list)

(define (d_apply p d w f x) = (apply f (append (list p d w) x)))

(define (d_merge x y) = (merge-state x y))

(define (d_equal x y) = (equal? x y))

(define (d_not_equal x y) = (not (equal? x y)))

(define (d_not x) = (not x))

(define (d_greater x y) = (> x y))

(define (d_less x y) = (< x y))

(define (d_sum x y) = (+ x y))

(define (d_minus x y) = (- x y))

(define (d_neg x) = (- x))

(define (d_multiply x y) = (* x y))

(define (d_expt x y) = (expt x y))

(define (d_exp x) = (exp x))

(define (d_sqrt x) = (sqrt x))

(define (d_divide x y) = (/ x y))

(define (d_random) = (random))

(define (d_random_integer n) = (random n))

(define (d_mod x y) = (modulo x y))

(define (d_div x y) = (quotient x y))

(define (d_round x) = (inexact->exact (round x)))

(define (d_floor x) = (inexact->exact (floor x)))

(define (d_ceiling x) = (inexact->exact (ceiling x)))

(define (d_cons x y) = (cons x y))

(define (d_first x) = (first x))

(define (d_rest x) = (rest x))

(define (d_empty_list) = '())

(define (d_list_length x) = (length x))

(define (d_make_array n) = (make-vector n))

(define (d_array_from_list list) = (list->vector list))

(define (d_array_length x) = (vector-length x))

(define (d_get x i) = (vector-ref x i))

(define (d_set x i v) = (vector-set! x i v) (empty-state))

(define (d_print_line x) =
  (displayln x)
  (flush-output (current-output-port))
  (empty-state))

(define (d_write_line h x) = (displayln x h) (flush-output h) (empty-state))

(define (d_write h x) = (display x h) (flush-output h) (empty-state))

(define (d_print x) = (display x) (flush-output (current-output-port)) (empty-state))

(define (d_read_num x) = (string->number x))

(define (d_ask_line) = (read-line (current-input-port) 'any))

(define (d_format x y) = 
  (define (read-identifier s)
    (format "l_~A"
    (list->string
    (let loop ()
      (let ((next (peek-char s)))
        (if (and (not (eof-object? next))
                 (or (char-alphabetic? next)
                     (char-numeric? next)
                     (char=? next #\_)))
            (cons (read-char s)
                  (loop))
            '()))))))
  (let ((in (open-input-string x))
        (out (open-output-string)))
    (let loop ()
      (let ((char (read-char in)))
        (unless (eof-object? char)
          (if (char=? char #\$)
              (let ((next (peek-char in)))
                (cond ((eof-object? next))
                      ((char=? next #\$)
                       (write-char (read-char in) out))
                      ((char=? next #\()
                       (read-char in)
                       (write-string (string->symbol (read-identifier in)) out)
                       (read-char in))
                      ((or (char-alphabetic? next)
                           (char=? next #\_))
                       (let ((var (string->symbol (read-identifier in))))
                         (let ((value (lookup var y)))
                           (if value
                               (display value out)
                               (write-string (format "'~A not found'" var) out)))))))
              (write-char char out))
          (loop))))
    (get-output-string out)))

(define (d_sleep n) = (sleep n) (empty-state))

(define (d_now) = (current-milliseconds))

(define (d_concatenate x y) = (string-append x y))

(define (d_split_string x y) = (regexp-split x y))

(define (d_string char-list) (apply string char-list))

(define d_characters string->list)

(define (d_throw text) (error text))

(define (d_home_dir) = (find-system-path 'home-dir))

(define (d_run_shell_command x) = (system x) (empty-state))

;; ----------------------------------------------------------------------------
;; Concurrency Data Structures
;; ----------------------------------------------------------------------------

(struct place ([value #:mutable] [ready? #:mutable]))

(define (finalize-place! place value) (set-place-value! place value) (set-place-ready?! place #t))

(define (make-place) (place #f #f))


(struct collective (places semaphore [error #:mutable]))

(define (make-collective place semaphore)
  (collective (mlist place) semaphore #f))

(define (push-new-place place collective) 
  (let ((new (make-place)))
    (let ((prev (mmember place (collective-places collective))))
      (set-mcdr! prev (mcons (mcar prev) (mcdr prev)))
      (set-mcar! prev new))
    new))

(struct link (place collective))

(define (make-link place collective) (link place collective))

;; ----------------------------------------------------------------------------
;; Collective Implementation
;; ----------------------------------------------------------------------------

;; called in para in fork and non-fork to run a thread
(define (run-child link fun) ;eigenlijk een collective ontvangen ipv link!?
  (thread (lambda () (with-handlers ((exn:fail? (lambda (x) (collective-kill (link-collective link) x))))
                         (fun)))))

;; helper for run-child
(define (collective-kill collective err)
  (set-collective-error! collective err)
  (semaphore-post (collective-semaphore collective)))

;; called in para in a not fork (new collective)
(define (setup-collective)
  (let ((place (make-place)))
    (make-link place (make-collective place (make-semaphore 0)))))

;; called in para to wait for the setup-collective
(define (wait-for-collective link) 
  (let ((collective (link-collective link)))
    (semaphore-wait (collective-semaphore collective))
    (if (collective-error collective)
        (raise (collective-error collective))
        (place-value (mcar (collective-places collective))))))

;; called in para in a fork
(define (join-collective link)
  (let ((collective (link-collective link)))
    (let ((new-place (push-new-place (link-place link) collective)))
      (make-link new-place collective))))

;; called when a thread terminates
(define (leave-collective link value)
  (let ((collective (link-collective link))
        (place (link-place link)))
    (finalize-place! place value)
    (collective-reduce-entries collective)
    (when (and (null? (mcdr (collective-places collective)))
               (place-ready? (mcar (collective-places collective))))
      (semaphore-post (collective-semaphore collective)))))

;; helper for leave-collective
(define (collective-reduce-entries collective)
  (do ((current (collective-places collective)))
    ((null? current) collective)
    (if (if (null? (mcdr current)) #f (and (place-ready? (mcar current))
                                          (place-ready? (mcar (mcdr current)))))
        (begin (set-place-value! (mcar current) (merge-state (place-value (mcar current))
                                                             (place-value (mcar (mcdr current)))))
               (set-mcdr! current (mcdr (mcdr current))))
        (set! current (mcdr current)))))

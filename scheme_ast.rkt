#lang racket

;; Generated code for Delta module 'scheme_ast' 

(require delta/runtime)
(require delta/standard)

(provide d_scheme_atom d_scheme_list d_gen_scheme_var d_scheme_true d_scheme_false d_scheme_empty_state d_scheme_quoted d_scheme_application d_scheme_single_state d_scheme_lookup d_scheme_lambda d_scheme_let d_scheme_if)

(define d_scheme_atom (lambda (l_p440 l_d441 l_l442 l_string) (let ((l_s439 (state (quote l_string) l_string))) (let ((l_t443 (let ((l_t444 (lookup (quote l_string) l_s439))) (let ((l_p445 l_p440)) (if l_p445 (merge-state l_p445 (if l_d441 (empty-state) l_t444)) (if l_d441 (empty-state) l_t444)))))) (if l_l442 (leave-collective l_l442 l_t443) l_t443)))))

(define d_scheme_list (lambda (l_p449 l_d450 l_l451 l_items) (let ((l_s448 (state (quote l_items) l_items))) (let ((l_t452 (let ((l_t453 (lookup (quote l_items) l_s448))) (let ((l_p454 l_p449)) (if l_p454 (merge-state l_p454 (if l_d450 (empty-state) l_t453)) (if l_d450 (empty-state) l_t453)))))) (if l_l451 (leave-collective l_l451 l_t452) l_t452)))))

(define d_gen_scheme_var (lambda (l_p458 l_d459 l_l460 l_x) (let ((l_s457 (state (quote l_x) l_x))) (d_scheme_atom l_p458 l_d459 l_l460 (d_gen_string #f #f #f (lookup (quote l_x) l_s457))))))

(define d_scheme_true (lambda (l_p466 l_d467 l_l468) (let ((l_s465 (state))) (d_scheme_atom l_p466 l_d467 l_l468 "#t"))))

(define d_scheme_false (lambda (l_p474 l_d475 l_l476) (let ((l_s473 (state))) (d_scheme_atom l_p474 l_d475 l_l476 "#f"))))

(define d_scheme_empty_state (lambda (l_p482 l_d483 l_l484) (let ((l_s481 (state))) (d_scheme_list l_p482 l_d483 l_l484 (d_cons (d_scheme_atom #f #f #f "empty-state") (d_empty_list))))))

(define d_scheme_quoted (lambda (l_p1269 l_d1270 l_l1271 l_x) (let ((l_s1268 (state (quote l_x) l_x))) (d_scheme_list l_p1269 l_d1270 l_l1271 (d_cons (d_scheme_atom #f #f #f "quote") (d_cons (lookup (quote l_x) l_s1268) (d_empty_list)))))))

(define d_scheme_application (lambda (l_p1285 l_d1286 l_l1287 l_name l_args) (let ((l_s1284 (state (quote l_name) l_name (quote l_args) l_args))) (d_scheme_list l_p1285 l_d1286 l_l1287 (d_cons (d_scheme_atom #f #f #f (lookup (quote l_name) l_s1284)) (lookup (quote l_args) l_s1284))))))

(define d_scheme_single_state (lambda (l_p1297 l_d1298 l_l1299 l_name l_exp) (let ((l_s1296 (state (quote l_name) l_name (quote l_exp) l_exp))) (d_scheme_application l_p1297 l_d1298 l_l1299 "singleton-state" (d_cons (d_scheme_quoted #f #f #f (d_scheme_atom #f #f #f (lookup (quote l_name) l_s1296))) (d_cons (lookup (quote l_exp) l_s1296) (d_empty_list)))))))

(define d_scheme_lookup (lambda (l_p1315 l_d1316 l_l1317 l_name l_exp) (let ((l_s1314 (state (quote l_name) l_name (quote l_exp) l_exp))) (d_scheme_application l_p1315 l_d1316 l_l1317 "lookup" (d_cons (d_scheme_quoted #f #f #f (d_scheme_atom #f #f #f (lookup (quote l_name) l_s1314))) (d_cons (lookup (quote l_exp) l_s1314) (d_empty_list)))))))

(define d_scheme_lambda (lambda (l_p1333 l_d1334 l_l1335 l_args l_body) (let ((l_s1332 (state (quote l_args) l_args (quote l_body) l_body))) (d_scheme_list l_p1333 l_d1334 l_l1335 (d_cons (d_scheme_atom #f #f #f "lambda") (d_cons (lookup (quote l_args) l_s1332) (d_cons (lookup (quote l_body) l_s1332) (d_empty_list))))))))

(define d_scheme_let (lambda (l_p1353 l_d1354 l_l1355 l_name l_exp l_body) (let ((l_s1352 (state (quote l_name) l_name (quote l_exp) l_exp (quote l_body) l_body))) (d_scheme_list l_p1353 l_d1354 l_l1355 (d_cons (d_scheme_atom #f #f #f "let") (d_cons (d_scheme_list #f #f #f (d_cons (d_scheme_list #f #f #f (d_cons (lookup (quote l_name) l_s1352) (d_cons (lookup (quote l_exp) l_s1352) (d_empty_list)))) (d_empty_list))) (d_cons (lookup (quote l_body) l_s1352) (d_empty_list))))))))

(define d_scheme_if (lambda (l_p1385 l_d1386 l_l1387 l_condition l_then_part l_else_part) (let ((l_s1384 (state (quote l_condition) l_condition (quote l_then_part) l_then_part (quote l_else_part) l_else_part))) (d_scheme_list l_p1385 l_d1386 l_l1387 (d_cons (d_scheme_atom #f #f #f "if") (d_cons (lookup (quote l_condition) l_s1384) (d_cons (lookup (quote l_then_part) l_s1384) (d_cons (lookup (quote l_else_part) l_s1384) (d_empty_list)))))))))

(define d_gen_counter (d_array_from_list (d_cons 0 (d_empty_list))))

(define d_gen_string (lambda (l_p1419 l_d1420 l_l1421 l_prefix) (let ((l_s1418 (state (quote l_prefix) l_prefix))) (let ((l_s1422 (merge-state l_s1418 (d_set d_gen_counter 0 (+ (d_get d_gen_counter 0) 1))))) (let ((l_t1458 (let ((l_t1459 (d_format "l_$p$c" (let ((l_s1441 (singleton-state (quote l_p) (lookup (quote l_prefix) l_s1422)))) (let ((l_s1442 (merge-state l_s1422 l_s1441))) (let ((l_s1443 l_s1441)) (merge-state (or l_s1443 (empty-state)) (singleton-state (quote l_c) (d_get d_gen_counter 0))))))))) (let ((l_p1460 l_p1419)) (if l_p1460 (merge-state l_p1460 (if l_d1420 (empty-state) l_t1459)) (if l_d1420 (empty-state) l_t1459)))))) (if l_l1421 (leave-collective l_l1421 l_t1458) l_t1458))))))

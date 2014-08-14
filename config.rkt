#lang racket

;; Generated code for Delta module 'config' 

(require delta/runtime)
(require delta/standard)

(provide d_is_operator_character d_is_left_associative d_is_right_associative d_is_operator d_precedence d_is_primitive d_is_primitive_function d_is_primitive_procedure d_is_standard d_is_standard_function d_is_standard_procedure d_operator_name d_is_system_module)

(define d_operator_characters (d_characters ".~`!@#$%^&*+=-|;:/?<>"))

(define d_is_operator_character (lambda (l_p668 l_d669 l_l670 l_x) (let ((l_s667 (state (quote l_x) l_x))) (d_is_member l_p668 l_d669 l_l670 (lookup (quote l_x) l_s667) d_operator_characters))))

(define d_assoc_left (d_cons "*" (d_cons "+" (d_cons "-" (d_cons "/" (d_cons "." (d_empty_list)))))))

(define d_assoc_right (d_cons ":=" (d_cons ";" (d_cons "|" (d_cons "&" (d_cons "and" (d_cons "or" (d_empty_list))))))))

(define d_precedence_table (d_cons (d_cons ";" (d_cons "|" (d_cons "&" (d_empty_list)))) (d_cons (d_cons ":=" (d_empty_list)) (d_cons (d_cons "and" (d_cons "or" (d_empty_list))) (d_cons (d_cons "=" (d_cons "!=" (d_cons "<" (d_cons ">" (d_cons "<=" (d_cons ">=" (d_empty_list))))))) (d_cons (d_cons "+" (d_cons "-" (d_empty_list))) (d_cons (d_cons "*" (d_cons "/" (d_empty_list))) (d_cons (d_cons "^" (d_empty_list)) (d_cons (d_cons "." (d_empty_list)) (d_empty_list))))))))))

(define d_is_left_associative (lambda (l_p1570 l_d1571 l_l1572 l_x) (let ((l_s1569 (state (quote l_x) l_x))) (d_is_member l_p1570 l_d1571 l_l1572 (lookup (quote l_x) l_s1569) d_assoc_left))))

(define d_is_right_associative (lambda (l_p1580 l_d1581 l_l1582 l_x) (let ((l_s1579 (state (quote l_x) l_x))) (d_is_member l_p1580 l_d1581 l_l1582 (lookup (quote l_x) l_s1579) d_assoc_right))))

(define d_is_operator (lambda (l_p1590 l_d1591 l_l1592 l_x) (let ((l_s1589 (state (quote l_x) l_x))) (d_is_member l_p1590 l_d1591 l_l1592 (lookup (quote l_x) l_s1589) d_operators))))

(define d_operators (d_fold #f #f #f d_append (d_empty_list) d_precedence_table))

(define d_precedence (lambda (l_p1608 l_d1609 l_l1610 l_x) (let ((l_s1607 (state (quote l_x) l_x))) (let ((l_s1611 (merge-state l_s1607 (let ((l_s1612 (singleton-state (quote l_precedence) 0))) (let ((l_s1613 (merge-state l_s1607 l_s1612))) (let ((l_s1614 l_s1612)) (let ((l_s1619 (singleton-state (quote l_table) d_precedence_table))) (let ((l_s1620 (merge-state l_s1613 l_s1619))) (let ((l_s1621 (merge-state (or l_s1614 (empty-state)) l_s1619))) (let ((l_s1626 (singleton-state (quote l_found) #f))) (let ((l_s1627 (merge-state l_s1620 l_s1626))) (let ((l_s1628 (merge-state (or l_s1621 (empty-state)) l_s1626))) (let ((l_s1633 (let ((l_s1636 (empty-state))) (let ((l_s1637 l_s1627)) (begin (while (if (not (lookup (quote l_found) l_s1637)) (d_not_equal (lookup (quote l_table) l_s1637) (d_empty_list)) #f) (let ((l_s1638 (if (d_is_member #f #f #f (lookup (quote l_x) l_s1637) (d_first (lookup (quote l_table) l_s1637))) (singleton-state (quote l_found) #t) (let ((l_s1661 (singleton-state (quote l_table) (d_rest (lookup (quote l_table) l_s1637))))) (let ((l_s1662 (merge-state l_s1637 l_s1661))) (let ((l_s1663 l_s1661)) (merge-state (or l_s1663 (empty-state)) (singleton-state (quote l_precedence) (+ (lookup (quote l_precedence) l_s1662) 1))))))))) (begin (set! l_s1636 (merge-state l_s1636 l_s1638)) (set! l_s1637 (merge-state l_s1637 l_s1638))))) l_s1636))))) (let ((l_s1634 (merge-state l_s1627 l_s1633))) (let ((l_s1635 (merge-state (or l_s1628 (empty-state)) l_s1633))) (if (not (lookup (quote l_found) l_s1634)) (merge-state (or l_s1635 (empty-state)) (d_throw (d_format "operator $op unknown when asked for precedence" (singleton-state (quote l_op) (lookup (quote l_x) l_s1634))))) (or l_s1635 (empty-state)))))))))))))))))) (let ((l_t1706 (let ((l_t1707 (lookup (quote l_precedence) l_s1611))) (let ((l_p1708 l_p1608)) (if l_p1708 (merge-state l_p1708 (if l_d1609 (empty-state) l_t1707)) (if l_d1609 (empty-state) l_t1707)))))) (if l_l1610 (leave-collective l_l1610 l_t1706) l_t1706))))))

(define d_primitive_functions (d_cons "apply" (d_cons "tuple" (d_cons "merge" (d_cons "equal" (d_cons "not_equal" (d_cons "greater" (d_cons "less" (d_cons "not" (d_cons "sum" (d_cons "multiply" (d_cons "minus" (d_cons "neg" (d_cons "divide" (d_cons "mod" (d_cons "div" (d_cons "expt" (d_cons "exp" (d_cons "sqrt" (d_cons "ask_line" (d_cons "read_num" (d_cons "format" (d_cons "empty_list" (d_cons "cons" (d_cons "first" (d_cons "rest" (d_cons "list_length" (d_cons "array_from_list" (d_cons "make_array" (d_cons "array_length" (d_cons "get" (d_cons "concatenate" (d_cons "split_string" (d_cons "listen_on" (d_cons "accept" (d_cons "connect_to" (d_cons "receive_from" (d_cons "open_channel" (d_cons "receive" (d_cons "channel_name" (d_cons "make_semaphore" (d_cons "open_input_file" (d_cons "open_output_file" (d_cons "read_char" (d_cons "peek_char" (d_cons "eof" (d_cons "whitespace" (d_cons "alphabetic" (d_cons "numeric" (d_cons "string" (d_cons "characters" (d_cons "file_position" (d_cons "file_range" (d_cons "file_lines" (d_cons "home_dir" (d_cons "make_window" (d_cons "random_integer" (d_cons "random" (d_cons "round" (d_cons "floor" (d_cons "ceiling" (d_cons "now" (d_cons "color" (d_cons "rgb" (d_empty_list)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(define d_primitive_procedures (d_cons "print" (d_cons "print_line" (d_cons "write" (d_cons "write_line" (d_cons "throw" (d_cons "set" (d_cons "sleep" (d_cons "send_to" (d_cons "close" (d_cons "send" (d_cons "up" (d_cons "down" (d_cons "run_shell_command" (d_cons "close_file" (d_cons "draw_point" (d_cons "draw_line" (d_cons "draw_rectangle" (d_cons "draw_ellipse" (d_cons "refresh" (d_cons "set_background" (d_empty_list))))))))))))))))))))))

(define d_standard_functions (d_cons "second" (d_cons "third" (d_cons "append" (d_cons "fold" (d_cons "reverse" (d_cons "is_member" (d_cons "map_list" (d_cons "filter_list" (d_cons "nth" (d_cons "union" (d_cons "intersection" (d_cons "difference" (d_cons "replace_suffix" (d_cons "intercalate" (d_cons "min" (d_cons "max" (d_cons "array_sum" (d_cons "naturals" (d_empty_list))))))))))))))))))))

(define d_standard_procedures (d_cons "swap" (d_cons "do_times" (d_cons "do_array" (d_cons "do_list" (d_empty_list))))))

(define d_is_primitive (lambda (l_p3126 l_d3127 l_l3128 l_x) (let ((l_s3125 (state (quote l_x) l_x))) (if (d_is_primitive_function #f #f #f (lookup (quote l_x) l_s3125)) (let ((l_t3131 (let ((l_t3132 #t)) (let ((l_p3133 l_p3126)) (if l_p3133 (merge-state l_p3133 (if l_d3127 (empty-state) l_t3132)) (if l_d3127 (empty-state) l_t3132)))))) (if l_l3128 (leave-collective l_l3128 l_t3131) l_t3131)) (d_is_primitive_procedure l_p3126 l_d3127 l_l3128 (lookup (quote l_x) l_s3125))))))

(define d_is_primitive_function (lambda (l_p3139 l_d3140 l_l3141 l_x) (let ((l_s3138 (state (quote l_x) l_x))) (d_is_member l_p3139 l_d3140 l_l3141 (lookup (quote l_x) l_s3138) d_primitive_functions))))

(define d_is_primitive_procedure (lambda (l_p3149 l_d3150 l_l3151 l_x) (let ((l_s3148 (state (quote l_x) l_x))) (d_is_member l_p3149 l_d3150 l_l3151 (lookup (quote l_x) l_s3148) d_primitive_procedures))))

(define d_is_standard (lambda (l_p3159 l_d3160 l_l3161 l_x) (let ((l_s3158 (state (quote l_x) l_x))) (if (d_is_standard_function #f #f #f (lookup (quote l_x) l_s3158)) (let ((l_t3164 (let ((l_t3165 #t)) (let ((l_p3166 l_p3159)) (if l_p3166 (merge-state l_p3166 (if l_d3160 (empty-state) l_t3165)) (if l_d3160 (empty-state) l_t3165)))))) (if l_l3161 (leave-collective l_l3161 l_t3164) l_t3164)) (d_is_standard_procedure l_p3159 l_d3160 l_l3161 (lookup (quote l_x) l_s3158))))))

(define d_is_standard_function (lambda (l_p4710 l_d4711 l_l4712 l_x) (let ((l_s4709 (state (quote l_x) l_x))) (d_is_member l_p4710 l_d4711 l_l4712 (lookup (quote l_x) l_s4709) d_standard_functions))))

(define d_is_standard_procedure (lambda (l_p4720 l_d4721 l_l4722 l_x) (let ((l_s4719 (state (quote l_x) l_x))) (d_is_member l_p4720 l_d4721 l_l4722 (lookup (quote l_x) l_s4719) d_standard_procedures))))

(define d_operator_names (d_cons (d_cons "+" (d_cons "sum" (d_empty_list))) (d_cons (d_cons "-" (d_cons "minus" (d_empty_list))) (d_cons (d_cons "*" (d_cons "multiply" (d_empty_list))) (d_cons (d_cons "*" (d_cons "multiply" (d_empty_list))) (d_cons (d_cons "/" (d_cons "divide" (d_empty_list))) (d_cons (d_cons "^" (d_cons "expt" (d_empty_list))) (d_cons (d_cons "=" (d_cons "equal" (d_empty_list))) (d_cons (d_cons "!=" (d_cons "not_equal" (d_empty_list))) (d_cons (d_cons ">" (d_cons "greater" (d_empty_list))) (d_cons (d_cons "<" (d_cons "less" (d_empty_list))) (d_cons (d_cons "and" (d_cons "and" (d_empty_list))) (d_cons (d_cons "or" (d_cons "or" (d_empty_list))) (d_empty_list))))))))))))))

(define d_operator_name (lambda (l_p4878 l_d4879 l_l4880 l_op) (let ((l_s4877 (state (quote l_op) l_op))) (let ((l_s4881 (merge-state l_s4877 (let ((l_s4882 (singleton-state (quote l_names) d_operator_names))) (let ((l_s4883 (merge-state l_s4877 l_s4882))) (let ((l_s4884 l_s4882)) (let ((l_s4889 (empty-state))) (let ((l_s4890 l_s4883)) (begin (while (if (d_not_equal (lookup (quote l_names) l_s4890) (d_empty_list)) (d_not_equal (d_first (d_first (lookup (quote l_names) l_s4890))) (lookup (quote l_op) l_s4890)) #f) (let ((l_s4891 (singleton-state (quote l_names) (d_rest (lookup (quote l_names) l_s4890))))) (begin (set! l_s4889 (merge-state l_s4889 l_s4891)) (set! l_s4890 (merge-state l_s4890 l_s4891))))) (merge-state (or l_s4884 (empty-state)) l_s4889)))))))))) (if (equal? (lookup (quote l_names) l_s4881) (d_empty_list)) (let ((l_t4934 (let ((l_t4935 (d_throw (d_format "operator $op not found in operator_names table" (singleton-state (quote l_op) (lookup (quote l_op) l_s4881)))))) (let ((l_p4936 l_p4878)) (if l_p4936 (merge-state l_p4936 (if l_d4879 (empty-state) l_t4935)) (if l_d4879 (empty-state) l_t4935)))))) (if l_l4880 (leave-collective l_l4880 l_t4934) l_t4934)) (d_second l_p4878 l_d4879 l_l4880 (d_first (lookup (quote l_names) l_s4881))))))))

(define d_system_modules (d_cons "io" (d_cons "channel" (d_cons "semaphore" (d_cons "socket" (d_cons "gui" (d_empty_list)))))))

(define d_is_system_module (lambda (l_p4968 l_d4969 l_l4970 l_x) (let ((l_s4967 (state (quote l_x) l_x))) (d_is_member l_p4968 l_d4969 l_l4970 (lookup (quote l_x) l_s4967) d_system_modules))))

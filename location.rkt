#lang racket

;; Generated code for Delta module 'location' 

(require delta/runtime)
(require delta/standard)
(require delta/io)

(provide d_make_location d_no_location d_location_text d_make_range d_no_range d_combine_ranges d_range_text d_throw_with_location d_throw_with_range)

(define d_make_location (lambda (l_p596 l_d597 l_l598 l_file l_row l_column l_position) (let ((l_s595 (state (quote l_file) l_file (quote l_row) l_row (quote l_column) l_column (quote l_position) l_position))) (let ((l_t607 (let ((l_t608 (d_tuple (lookup (quote l_file) l_s595) (lookup (quote l_row) l_s595) (lookup (quote l_column) l_s595) (lookup (quote l_position) l_s595)))) (let ((l_p609 l_p596)) (if l_p609 (merge-state l_p609 (if l_d597 (empty-state) l_t608)) (if l_d597 (empty-state) l_t608)))))) (if l_l598 (leave-collective l_l598 l_t607) l_t607)))))

(define d_no_location (lambda (l_p613 l_d614 l_l615) (let ((l_s612 (state))) (let ((l_t624 (let ((l_t625 (d_tuple "" 0 0 0))) (let ((l_p626 l_p613)) (if l_p626 (merge-state l_p626 (if l_d614 (empty-state) l_t625)) (if l_d614 (empty-state) l_t625)))))) (if l_l615 (leave-collective l_l615 l_t624) l_t624)))))

(define d_location_file (lambda (l_p630 l_d631 l_l632 l_location) (let ((l_s629 (state (quote l_location) l_location))) (d_apply l_p630 l_d631 l_l632 (lambda (l_p1462 l_d1463 l_l1464 l_a l_b l_c l_d) (let ((l_s1461 (merge-state l_s629 (state (quote l_a) l_a (quote l_b) l_b (quote l_c) l_c (quote l_d) l_d)))) (let ((l_t1465 (let ((l_t1466 (lookup (quote l_a) l_s1461))) (let ((l_p1467 l_p1462)) (if l_p1467 (merge-state l_p1467 (if l_d1463 (empty-state) l_t1466)) (if l_d1463 (empty-state) l_t1466)))))) (if l_l1464 (leave-collective l_l1464 l_t1465) l_t1465)))) (lookup (quote l_location) l_s629)))))

(define d_location_row (lambda (l_p1473 l_d1474 l_l1475 l_location) (let ((l_s1472 (state (quote l_location) l_location))) (d_apply l_p1473 l_d1474 l_l1475 (lambda (l_p1477 l_d1478 l_l1479 l_a l_b l_c l_d) (let ((l_s1476 (merge-state l_s1472 (state (quote l_a) l_a (quote l_b) l_b (quote l_c) l_c (quote l_d) l_d)))) (let ((l_t1480 (let ((l_t2766 (lookup (quote l_b) l_s1476))) (let ((l_p2767 l_p1477)) (if l_p2767 (merge-state l_p2767 (if l_d1478 (empty-state) l_t2766)) (if l_d1478 (empty-state) l_t2766)))))) (if l_l1479 (leave-collective l_l1479 l_t1480) l_t1480)))) (lookup (quote l_location) l_s1472)))))

(define d_location_column (lambda (l_p2773 l_d2774 l_l2775 l_location) (let ((l_s2772 (state (quote l_location) l_location))) (d_apply l_p2773 l_d2774 l_l2775 (lambda (l_p2777 l_d2778 l_l2779 l_a l_b l_c l_d) (let ((l_s2776 (merge-state l_s2772 (state (quote l_a) l_a (quote l_b) l_b (quote l_c) l_c (quote l_d) l_d)))) (let ((l_t2780 (let ((l_t2781 (lookup (quote l_c) l_s2776))) (let ((l_p2782 l_p2777)) (if l_p2782 (merge-state l_p2782 (if l_d2778 (empty-state) l_t2781)) (if l_d2778 (empty-state) l_t2781)))))) (if l_l2779 (leave-collective l_l2779 l_t2780) l_t2780)))) (lookup (quote l_location) l_s2772)))))

(define d_location_position (lambda (l_p2788 l_d2789 l_l2790 l_location) (let ((l_s2787 (state (quote l_location) l_location))) (d_apply l_p2788 l_d2789 l_l2790 (lambda (l_p2792 l_d2793 l_l2794 l_a l_b l_c l_d) (let ((l_s2791 (merge-state l_s2787 (state (quote l_a) l_a (quote l_b) l_b (quote l_c) l_c (quote l_d) l_d)))) (let ((l_t2795 (let ((l_t2796 (lookup (quote l_d) l_s2791))) (let ((l_p2797 l_p2792)) (if l_p2797 (merge-state l_p2797 (if l_d2793 (empty-state) l_t2796)) (if l_d2793 (empty-state) l_t2796)))))) (if l_l2794 (leave-collective l_l2794 l_t2795) l_t2795)))) (lookup (quote l_location) l_s2787)))))

(define d_location_text (lambda (l_p2803 l_d2804 l_l2805 l_location) (let ((l_s2802 (state (quote l_location) l_location))) (d_source_indicator l_p2803 l_d2804 l_l2805 (d_location_file #f #f #f (lookup (quote l_location) l_s2802)) (d_location_row #f #f #f (lookup (quote l_location) l_s2802)) (d_location_column #f #f #f (lookup (quote l_location) l_s2802)) (d_location_row #f #f #f (lookup (quote l_location) l_s2802)) (+ (d_location_column #f #f #f (lookup (quote l_location) l_s2802)) 1)))))

(define d_make_range (lambda (l_p2823 l_d2824 l_l2825 l_from l_to) (let ((l_s2822 (state (quote l_from) l_from (quote l_to) l_to))) (let ((l_t2830 (let ((l_t2831 (d_tuple (lookup (quote l_from) l_s2822) (lookup (quote l_to) l_s2822)))) (let ((l_p2832 l_p2823)) (if l_p2832 (merge-state l_p2832 (if l_d2824 (empty-state) l_t2831)) (if l_d2824 (empty-state) l_t2831)))))) (if l_l2825 (leave-collective l_l2825 l_t2830) l_t2830)))))

(define d_no_range (lambda (l_p2836 l_d2837 l_l2838) (let ((l_s2835 (state))) (d_make_range l_p2836 l_d2837 l_l2838 (d_no_location #f #f #f) (d_no_location #f #f #f)))))

(define d_combine_ranges (lambda (l_p2842 l_d2843 l_l2844 l_from l_to) (let ((l_s2841 (state (quote l_from) l_from (quote l_to) l_to))) (d_make_range l_p2842 l_d2843 l_l2844 (d_range_from #f #f #f (lookup (quote l_from) l_s2841)) (d_range_to #f #f #f (lookup (quote l_to) l_s2841))))))

(define d_range_from (lambda (l_p2852 l_d2853 l_l2854 l_range) (let ((l_s2851 (state (quote l_range) l_range))) (d_apply l_p2852 l_d2853 l_l2854 (lambda (l_p2856 l_d2857 l_l2858 l_a l_b) (let ((l_s2855 (merge-state l_s2851 (state (quote l_a) l_a (quote l_b) l_b)))) (let ((l_t2859 (let ((l_t2860 (lookup (quote l_a) l_s2855))) (let ((l_p2861 l_p2856)) (if l_p2861 (merge-state l_p2861 (if l_d2857 (empty-state) l_t2860)) (if l_d2857 (empty-state) l_t2860)))))) (if l_l2858 (leave-collective l_l2858 l_t2859) l_t2859)))) (lookup (quote l_range) l_s2851)))))

(define d_range_to (lambda (l_p2867 l_d2868 l_l2869 l_range) (let ((l_s2866 (state (quote l_range) l_range))) (d_apply l_p2867 l_d2868 l_l2869 (lambda (l_p2871 l_d2872 l_l2873 l_a l_b) (let ((l_s2870 (merge-state l_s2866 (state (quote l_a) l_a (quote l_b) l_b)))) (let ((l_t2874 (let ((l_t2875 (lookup (quote l_b) l_s2870))) (let ((l_p2876 l_p2871)) (if l_p2876 (merge-state l_p2876 (if l_d2872 (empty-state) l_t2875)) (if l_d2872 (empty-state) l_t2875)))))) (if l_l2873 (leave-collective l_l2873 l_t2874) l_t2874)))) (lookup (quote l_range) l_s2866)))))

(define d_range_text (lambda (l_p2882 l_d2883 l_l2884 l_range) (let ((l_s2881 (state (quote l_range) l_range))) (d_source_indicator l_p2882 l_d2883 l_l2884 (d_location_file #f #f #f (d_range_from #f #f #f (lookup (quote l_range) l_s2881))) (d_location_row #f #f #f (d_range_from #f #f #f (lookup (quote l_range) l_s2881))) (d_location_column #f #f #f (d_range_from #f #f #f (lookup (quote l_range) l_s2881))) (d_location_row #f #f #f (d_range_to #f #f #f (lookup (quote l_range) l_s2881))) (d_location_column #f #f #f (d_range_to #f #f #f (lookup (quote l_range) l_s2881)))))))

(define d_source_indicator (lambda (l_p4323 l_d4324 l_l4325 l_file l_from_row l_from_column l_to_row l_to_column) (let ((l_s4322 (state (quote l_file) l_file (quote l_from_row) l_from_row (quote l_from_column) l_from_column (quote l_to_row) l_to_row (quote l_to_column) l_to_column))) (let ((l_t4685 (let ((l_t4686 (try-catch (lambda () (let ((l_s4376 (merge-state l_s4322 (let ((l_s4377 (singleton-state (quote l_handle) (d_open_input_file (lookup (quote l_file) l_s4322))))) (let ((l_s4378 (merge-state l_s4322 l_s4377))) (let ((l_s4379 l_s4377)) (let ((l_s4386 (singleton-state (quote l_file_lines) (d_file_lines (lookup (quote l_handle) l_s4378))))) (let ((l_s4387 (merge-state l_s4378 l_s4386))) (let ((l_s4388 (merge-state (or l_s4379 (empty-state)) l_s4386))) (let ((l_s4395 (d_close_file (lookup (quote l_handle) l_s4387)))) (let ((l_s4396 (merge-state l_s4387 l_s4395))) (let ((l_s4397 (merge-state (or l_s4388 (empty-state)) l_s4395))) (let ((l_s4402 (singleton-state (quote l_lines) (d_cons (d_nth #f #f #f (- (lookup (quote l_from_row) l_s4396) 1) (lookup (quote l_file_lines) l_s4396)) (d_empty_list))))) (let ((l_s4403 (merge-state l_s4396 l_s4402))) (let ((l_s4404 (merge-state (or l_s4397 (empty-state)) l_s4402))) (let ((l_s4419 (singleton-state (quote l_i) (- (lookup (quote l_from_row) l_s4403) 1)))) (let ((l_s4420 (merge-state l_s4403 l_s4419))) (let ((l_s4421 (merge-state (or l_s4404 (empty-state)) l_s4419))) (let ((l_s4430 (empty-state))) (let ((l_s4431 l_s4420)) (begin (while (d_less (lookup (quote l_i) l_s4431) (- (lookup (quote l_to_row) l_s4431) 1)) (let ((l_s4432 (let ((l_s4443 (singleton-state (quote l_i) (+ (lookup (quote l_i) l_s4431) 1)))) (let ((l_s4444 (merge-state l_s4431 l_s4443))) (let ((l_s4445 l_s4443)) (merge-state (or l_s4445 (empty-state)) (singleton-state (quote l_lines) (d_cons (d_nth #f #f #f (lookup (quote l_i) l_s4444) (lookup (quote l_file_lines) l_s4444)) (lookup (quote l_lines) l_s4444))))))))) (begin (set! l_s4430 (merge-state l_s4430 l_s4432)) (set! l_s4431 (merge-state l_s4431 l_s4432))))) (merge-state (or l_s4421 (empty-state)) l_s4430)))))))))))))))))))))) (if (d_less (lookup (quote l_from_row) l_s4376) (lookup (quote l_to_row) l_s4376)) (d_format "At lines $from to $to in file $file:\n$text\n" (let ((l_s4486 (singleton-state (quote l_from) (lookup (quote l_from_row) l_s4376)))) (let ((l_s4487 (merge-state l_s4376 l_s4486))) (let ((l_s4488 l_s4486)) (let ((l_s4493 (singleton-state (quote l_to) (lookup (quote l_to_row) l_s4487)))) (let ((l_s4494 (merge-state l_s4487 l_s4493))) (let ((l_s4495 (merge-state (or l_s4488 (empty-state)) l_s4493))) (let ((l_s4500 (singleton-state (quote l_file) (lookup (quote l_file) l_s4494)))) (let ((l_s4501 (merge-state l_s4494 l_s4500))) (let ((l_s4502 (merge-state (or l_s4495 (empty-state)) l_s4500))) (merge-state (or l_s4502 (empty-state)) (singleton-state (quote l_text) (d_fold #f #f #f (lambda (l_p4508 l_d4509 l_l4510 l_x l_y) (let ((l_s4507 (merge-state l_s4501 (state (quote l_x) l_x (quote l_y) l_y)))) (let ((l_t4526 (let ((l_t4527 (d_format "$x$y" (let ((l_s4513 (singleton-state (quote l_x) (lookup (quote l_x) l_s4507)))) (let ((l_s4514 (merge-state l_s4507 l_s4513))) (let ((l_s4515 l_s4513)) (merge-state (or l_s4515 (empty-state)) (singleton-state (quote l_y) (lookup (quote l_y) l_s4514))))))))) (let ((l_p4528 l_p4508)) (if l_p4528 (merge-state l_p4528 (if l_d4509 (empty-state) l_t4527)) (if l_d4509 (empty-state) l_t4527)))))) (if l_l4510 (leave-collective l_l4510 l_t4526) l_t4526)))) "" (d_map_list #f #f #f (lambda (l_p4532 l_d4533 l_l4534 l_x) (let ((l_s4531 (merge-state l_s4501 (state (quote l_x) l_x)))) (let ((l_t4541 (let ((l_t4542 (d_format "\n> $x" (singleton-state (quote l_x) (lookup (quote l_x) l_s4531))))) (let ((l_p4543 l_p4532)) (if l_p4543 (merge-state l_p4543 (if l_d4533 (empty-state) l_t4542)) (if l_d4533 (empty-state) l_t4542)))))) (if l_l4534 (leave-collective l_l4534 l_t4541) l_t4541)))) (d_reverse #f #f #f (lookup (quote l_lines) l_s4501)))))))))))))))) (let ((l_s4556 (merge-state l_s4376 (let ((l_s4557 (singleton-state (quote l_i) 0))) (let ((l_s4558 (merge-state l_s4376 l_s4557))) (let ((l_s4559 l_s4557)) (let ((l_s4564 (singleton-state (quote l_indicator) ""))) (let ((l_s4565 (merge-state l_s4558 l_s4564))) (let ((l_s4566 (merge-state (or l_s4559 (empty-state)) l_s4564))) (let ((l_s4571 (let ((l_s4574 (empty-state))) (let ((l_s4575 l_s4565)) (begin (while (d_not_equal (lookup (quote l_i) l_s4575) (lookup (quote l_from_column) l_s4575)) (let ((l_s4576 (let ((l_s4583 (singleton-state (quote l_indicator) (d_format "$x " (singleton-state (quote l_x) (lookup (quote l_indicator) l_s4575)))))) (let ((l_s4584 (merge-state l_s4575 l_s4583))) (let ((l_s4585 l_s4583)) (merge-state (or l_s4585 (empty-state)) (singleton-state (quote l_i) (+ (lookup (quote l_i) l_s4584) 1)))))))) (begin (set! l_s4574 (merge-state l_s4574 l_s4576)) (set! l_s4575 (merge-state l_s4575 l_s4576))))) l_s4574))))) (let ((l_s4572 (merge-state l_s4565 l_s4571))) (let ((l_s4573 (merge-state (or l_s4566 (empty-state)) l_s4571))) (let ((l_s4608 (empty-state))) (let ((l_s4609 l_s4572)) (begin (while (d_not_equal (lookup (quote l_i) l_s4609) (lookup (quote l_to_column) l_s4609)) (let ((l_s4610 (let ((l_s4617 (singleton-state (quote l_indicator) (d_format "$x^" (singleton-state (quote l_x) (lookup (quote l_indicator) l_s4609)))))) (let ((l_s4618 (merge-state l_s4609 l_s4617))) (let ((l_s4619 l_s4617)) (merge-state (or l_s4619 (empty-state)) (singleton-state (quote l_i) (+ (lookup (quote l_i) l_s4618) 1)))))))) (begin (set! l_s4608 (merge-state l_s4608 l_s4610)) (set! l_s4609 (merge-state l_s4609 l_s4610))))) (merge-state (or l_s4573 (empty-state)) l_s4608)))))))))))))))) (d_format "At line $line in file $file:\n\n$text\n$ind" (let ((l_s4650 (singleton-state (quote l_line) (lookup (quote l_from_row) l_s4556)))) (let ((l_s4651 (merge-state l_s4556 l_s4650))) (let ((l_s4652 l_s4650)) (let ((l_s4657 (singleton-state (quote l_file) (lookup (quote l_file) l_s4651)))) (let ((l_s4658 (merge-state l_s4651 l_s4657))) (let ((l_s4659 (merge-state (or l_s4652 (empty-state)) l_s4657))) (let ((l_s4664 (singleton-state (quote l_text) (d_first (lookup (quote l_lines) l_s4658))))) (let ((l_s4665 (merge-state l_s4658 l_s4664))) (let ((l_s4666 (merge-state (or l_s4659 (empty-state)) l_s4664))) (merge-state (or l_s4666 (empty-state)) (singleton-state (quote l_ind) (lookup (quote l_indicator) l_s4665))))))))))))))))) (lambda (l_x) (let ((l_s4326 (merge-state l_s4322 (singleton-state (quote l_x) (exn-message l_x))))) (try-catch (lambda () (d_format "At lines $from (pos = $x) to $to (pos = $y) in file $file" (let ((l_s4332 (singleton-state (quote l_from) (lookup (quote l_from_row) l_s4326)))) (let ((l_s4333 (merge-state l_s4326 l_s4332))) (let ((l_s4334 l_s4332)) (let ((l_s4339 (singleton-state (quote l_x) (lookup (quote l_from_column) l_s4333)))) (let ((l_s4340 (merge-state l_s4333 l_s4339))) (let ((l_s4341 (merge-state (or l_s4334 (empty-state)) l_s4339))) (let ((l_s4346 (singleton-state (quote l_to) (lookup (quote l_to_row) l_s4340)))) (let ((l_s4347 (merge-state l_s4340 l_s4346))) (let ((l_s4348 (merge-state (or l_s4341 (empty-state)) l_s4346))) (let ((l_s4353 (singleton-state (quote l_y) (lookup (quote l_to_column) l_s4347)))) (let ((l_s4354 (merge-state l_s4347 l_s4353))) (let ((l_s4355 (merge-state (or l_s4348 (empty-state)) l_s4353))) (merge-state (or l_s4355 (empty-state)) (singleton-state (quote l_file) (lookup (quote l_file) l_s4354))))))))))))))))) (lambda (l_x) (let ((l_s4327 (merge-state l_s4326 (singleton-state (quote l_x) (exn-message l_x))))) "No source code location available.")))))))) (let ((l_p4687 l_p4323)) (if l_p4687 (merge-state l_p4687 (if l_d4324 (empty-state) l_t4686)) (if l_d4324 (empty-state) l_t4686)))))) (if l_l4325 (leave-collective l_l4325 l_t4685) l_t4685)))))

(define d_throw_with_location (lambda (l_p4691 l_d4692 l_l4693 l_string l_state l_location) (let ((l_s4690 (state (quote l_string) l_string (quote l_state) l_state (quote l_location) l_location))) (let ((l_t5629 (let ((l_t5630 (d_throw (d_format "\n$loc\n\n  $message\n" (let ((l_s4696 (singleton-state (quote l_loc) (d_location_text #f #f #f (lookup (quote l_location) l_s4690))))) (let ((l_s4697 (merge-state l_s4690 l_s4696))) (let ((l_s4698 l_s4696)) (merge-state (or l_s4698 (empty-state)) (singleton-state (quote l_message) (d_format (lookup (quote l_string) l_s4697) (lookup (quote l_state) l_s4697))))))))))) (or l_p4691 (empty-state))))) (if l_l4693 (leave-collective l_l4693 l_t5629) l_t5629)))))

(define d_throw_with_range (lambda (l_p5635 l_d5636 l_l5637 l_string l_state l_range) (let ((l_s5634 (state (quote l_string) l_string (quote l_state) l_state (quote l_range) l_range))) (let ((l_t5659 (let ((l_t5660 (d_throw (d_format "\n$loc\n\n  $message\n" (let ((l_s5640 (singleton-state (quote l_loc) (d_range_text #f #f #f (lookup (quote l_range) l_s5634))))) (let ((l_s5641 (merge-state l_s5634 l_s5640))) (let ((l_s5642 l_s5640)) (merge-state (or l_s5642 (empty-state)) (singleton-state (quote l_message) (d_format (lookup (quote l_string) l_s5641) (lookup (quote l_state) l_s5641))))))))))) (or l_p5635 (empty-state))))) (if l_l5637 (leave-collective l_l5637 l_t5659) l_t5659)))))
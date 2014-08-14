#lang racket

;; Generated code for Delta module 'delta' 

(require delta/runtime)
(require delta/standard)
(require delta/io)
(require "compiler.rkt")

(provide d_main)

(define d_main (lambda (l_p6 l_d7 l_l8 l_args) (let ((l_s5 (state (quote l_args) l_args))) (if (equal? (lookup (quote l_args) l_s5) (d_empty_list)) (d_handle_incorrect_command l_p6 #t l_l8) (let ((l_s15 (merge-state l_s5 (let ((l_s16 (singleton-state (quote l_verb) (d_first (lookup (quote l_args) l_s5))))) (let ((l_s17 (merge-state l_s5 l_s16))) (let ((l_s18 l_s16)) (merge-state (or l_s18 (empty-state)) (singleton-state (quote l_command_args) (d_rest (lookup (quote l_args) l_s17)))))))))) (if (equal? (lookup (quote l_verb) l_s15) "help") (d_handle_help_command l_p6 #t l_l8 (lookup (quote l_command_args) l_s15)) (if (equal? (lookup (quote l_verb) l_s15) "run") (d_handle_run_command l_p6 #t l_l8 (lookup (quote l_command_args) l_s15)) (if (equal? (lookup (quote l_verb) l_s15) "compile") (d_handle_compile_command l_p6 #t l_l8 (lookup (quote l_command_args) l_s15)) (if (equal? (lookup (quote l_verb) l_s15) "exe") (d_handle_exe_command l_p6 #t l_l8 (lookup (quote l_command_args) l_s15)) (d_handle_incorrect_command l_p6 #t l_l8))))))))))

(define d_handle_incorrect_command (lambda (l_p68 l_d69 l_l70) (let ((l_s67 (state))) (let ((l_s71 (d_print_logo #f #f #f))) (let ((l_s72 (merge-state l_s67 l_s71))) (let ((l_s73 (let ((l_t79 l_s71)) (or l_p68 (empty-state))))) (let ((l_t76 (let ((l_t77 (d_print_line "Type 'delta help' for help"))) (or l_s73 (empty-state))))) (if l_l70 (leave-collective l_l70 l_t76) l_t76))))))))

(define d_handle_help_command (lambda (l_p84 l_d85 l_l86 l_args) (let ((l_s83 (state (quote l_args) l_args))) (let ((l_s87 (d_print_logo #f #f #f))) (let ((l_s88 (merge-state l_s83 l_s87))) (let ((l_s89 (let ((l_t132 l_s87)) (or l_p84 (empty-state))))) (if (equal? (lookup (quote l_args) l_s88) (d_empty_list)) (d_print_help l_s89 #t l_l86) (let ((l_s96 (merge-state l_s88 (singleton-state (quote l_topic) (d_first (lookup (quote l_args) l_s88)))))) (if (equal? (lookup (quote l_topic) l_s96) "run") (d_print_run_help l_s89 #t l_l86) (if (equal? (lookup (quote l_topic) l_s96) "compile") (d_print_compile_help l_s89 #t l_l86) (if (equal? (lookup (quote l_topic) l_s96) "exe") (d_print_exe_help l_s89 #t l_l86) (let ((l_t129 (let ((l_t130 (d_print_line (d_format "No help for '$x'" (singleton-state (quote l_x) (lookup (quote l_topic) l_s96)))))) (or l_s89 (empty-state))))) (if l_l86 (leave-collective l_l86 l_t129) l_t129)))))))))))))

(define d_handle_run_command (lambda (l_p137 l_d138 l_l139 l_args) (let ((l_s136 (state (quote l_args) l_args))) (if (equal? (lookup (quote l_args) l_s136) (d_empty_list)) (let ((l_t148 (let ((l_t149 (d_print_line "Command 'run' expects a file argument. Type 'delta help' for further help."))) (or l_p137 (empty-state))))) (if l_l139 (leave-collective l_l139 l_t148) l_t148)) (let ((l_s151 (merge-state l_s136 (let ((l_s152 (singleton-state (quote l_file) (d_first (lookup (quote l_args) l_s136))))) (let ((l_s153 (merge-state l_s136 l_s152))) (let ((l_s154 l_s152)) (merge-state (or l_s154 (empty-state)) (singleton-state (quote l_compiled) (d_compile_project #f #f #f (lookup (quote l_file) l_s153)))))))))) (let ((l_t754 (let ((l_t755 (d_run_shell_command (d_format "racket $x $y" (let ((l_s169 (singleton-state (quote l_x) (lookup (quote l_compiled) l_s151)))) (let ((l_s170 (merge-state l_s151 l_s169))) (let ((l_s171 l_s169)) (merge-state (or l_s171 (empty-state)) (singleton-state (quote l_y) (d_intercalate #f #f #f (d_rest (lookup (quote l_args) l_s170)) " ")))))))))) (or l_p137 (empty-state))))) (if l_l139 (leave-collective l_l139 l_t754) l_t754)))))))

(define d_handle_compile_command (lambda (l_p760 l_d761 l_l762 l_args) (let ((l_s759 (state (quote l_args) l_args))) (let ((l_s763 (d_print_logo #f #f #f))) (let ((l_s764 (merge-state l_s759 l_s763))) (let ((l_s765 (let ((l_t828 l_s763)) (or l_p760 (empty-state))))) (if (equal? (lookup (quote l_args) l_s764) (d_empty_list)) (let ((l_t774 (let ((l_t775 (d_print_line "Command 'compile' expects a file argument. Type 'delta help' for further help."))) (or l_s765 (empty-state))))) (if l_l762 (leave-collective l_l762 l_t774) l_t774)) (let ((l_s777 (merge-state l_s764 (singleton-state (quote l_file) (d_first (lookup (quote l_args) l_s764)))))) (let ((l_s784 (d_print_line (d_format "Compiling file $x" (singleton-state (quote l_x) (lookup (quote l_file) l_s777)))))) (let ((l_s785 (merge-state l_s777 l_s784))) (let ((l_s786 (let ((l_t826 l_s784)) (or l_s765 (empty-state))))) (let ((l_s797 (singleton-state (quote l_compiled) (d_compile_file #f #f #f (lookup (quote l_file) l_s785))))) (let ((l_s798 (merge-state l_s785 l_s797))) (let ((l_s799 (let ((l_t824 l_s797)) (or l_s786 (empty-state))))) (let ((l_s804 (d_print_line (d_format "Created file $x" (singleton-state (quote l_x) (lookup (quote l_compiled) l_s798)))))) (let ((l_s805 (merge-state l_s798 l_s804))) (let ((l_s806 (let ((l_t822 l_s804)) (or l_s799 (empty-state))))) (let ((l_t819 (let ((l_t820 (d_print_line "Ready"))) (or l_s806 (empty-state))))) (if l_l762 (leave-collective l_l762 l_t819) l_t819)))))))))))))))))))

(define d_handle_exe_command (lambda (l_p833 l_d834 l_l835 l_args) (let ((l_s832 (state (quote l_args) l_args))) (let ((l_s836 (d_print_logo #f #f #f))) (let ((l_s837 (merge-state l_s832 l_s836))) (let ((l_s838 (let ((l_t920 l_s836)) (or l_p833 (empty-state))))) (if (equal? (lookup (quote l_args) l_s837) (d_empty_list)) (let ((l_t847 (let ((l_t848 (d_print_line "Command 'exe' expects a file argument. Type 'delta help' for further help."))) (or l_s838 (empty-state))))) (if l_l835 (leave-collective l_l835 l_t847) l_t847)) (let ((l_s850 (merge-state l_s837 (singleton-state (quote l_file) (d_first (lookup (quote l_args) l_s837)))))) (let ((l_s857 (d_print_line (d_format "Compiling file $x\n" (singleton-state (quote l_x) (lookup (quote l_file) l_s850)))))) (let ((l_s858 (merge-state l_s850 l_s857))) (let ((l_s859 (let ((l_t918 l_s857)) (or l_s838 (empty-state))))) (let ((l_s870 (singleton-state (quote l_compiled) (d_compile_project #f #f #f (lookup (quote l_file) l_s858))))) (let ((l_s871 (merge-state l_s858 l_s870))) (let ((l_s872 (let ((l_t916 l_s870)) (or l_s859 (empty-state))))) (let ((l_s877 (d_print_line (d_format "Creating executable file $x" (singleton-state (quote l_x) (d_replace_suffix #f #f #f (lookup (quote l_compiled) l_s871) "")))))) (let ((l_s878 (merge-state l_s871 l_s877))) (let ((l_s879 (let ((l_t914 l_s877)) (or l_s872 (empty-state))))) (let ((l_s892 (d_run_shell_command (d_format "raco exe $x" (singleton-state (quote l_x) (d_replace_suffix #f #f #f (lookup (quote l_file) l_s878) ".rkt")))))) (let ((l_s893 (merge-state l_s878 l_s892))) (let ((l_s894 (let ((l_t912 l_s892)) (or l_s879 (empty-state))))) (let ((l_t909 (let ((l_t910 (d_print_line "Ready"))) (or l_s894 (empty-state))))) (if l_l835 (leave-collective l_l835 l_t909) l_t909))))))))))))))))))))))

(define d_print_logo (lambda (l_p925 l_d926 l_l927) (let ((l_s924 (state))) (let ((l_t930 (let ((l_t931 (d_print_line "\nDelta compiler version 0, Paul Griffioen 2012. Powered by Racket Scheme.\n"))) (or l_p925 (empty-state))))) (if l_l927 (leave-collective l_l927 l_t930) l_t930)))))

(define d_print_help (lambda (l_p1920 l_d1921 l_l1922) (let ((l_s1919 (state))) (let ((l_t1925 (let ((l_t1926 (d_print_line "The syntax is

  delta command arg1 arg2 ...

The following commands are supported:

  help            Display this help.
  help command    Display help for the command.
  compile file    Compile the file to Scheme.
  run file        Compile the file and its dependencies to Scheme
                  and run 'main'.
  exe file        Compile the file and its dependencies to Scheme 
                  and create an executable that runs 'main'.\n"))) (or l_p1920 (empty-state))))) (if l_l1922 (leave-collective l_l1922 l_t1925) l_t1925)))))

(define d_print_run_help (lambda (l_p1931 l_d1932 l_l1933) (let ((l_s1930 (state))) (let ((l_t1936 (let ((l_t1937 (d_print_line "The syntax is

  delta run file

Compiles the file and all its dependencies to Scheme and runs
'main'. Leaves .rkt files for the compiled files.\n"))) (or l_p1931 (empty-state))))) (if l_l1933 (leave-collective l_l1933 l_t1936) l_t1936)))))

(define d_print_compile_help (lambda (l_p1942 l_d1943 l_l1944) (let ((l_s1941 (state))) (let ((l_t1947 (let ((l_t1948 (d_print_line "The syntax is

  delta compile file

Compiles the file to Scheme. Creates a .rkt file with the same name as
the module name. See command racket for further information.\n"))) (or l_p1942 (empty-state))))) (if l_l1944 (leave-collective l_l1944 l_t1947) l_t1947)))))

(define d_print_exe_help (lambda (l_p1953 l_d1954 l_l1955) (let ((l_s1952 (state))) (let ((l_t1958 (let ((l_t1959 (d_print_line "The syntax is

  delta exe file

Compiles the file and all its dependencies to Scheme and create an
executable that runs 'main'. See raco for further information.\n"))) (or l_p1953 (empty-state))))) (if l_l1955 (leave-collective l_l1955 l_t1958) l_t1958)))))


(let ((dummy (with-handlers ((exn:fail? (lambda (l_err1961) (displayln (exn-message l_err1961))))) (d_main #f #f #f (vector->list (current-command-line-arguments)))))) (void))

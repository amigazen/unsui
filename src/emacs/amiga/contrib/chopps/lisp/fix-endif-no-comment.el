;;
;;  This small bit of lisp will fix the so common #endif somthing
;; (with no comment) code.
;;

(defun fix-endif-no-comment ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (replace-regexp "^#endif \\(.*\\)" "#endif /* \\1 */")
    (beginning-of-buffer)
    (replace-regexp "^#endif /\\* /\\* \\(.*\\) \\*/ \\*/" "#endif /* \\1 */")))

    
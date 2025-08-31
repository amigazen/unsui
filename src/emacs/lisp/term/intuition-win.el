(load "s:.emacs-menu" t t)
(setq command-switch-alist (append '(("-fn" . amiga-handle-set-font)
				     ("-fg" . amiga-handle-set-foreground)
				     ("-bg" . amiga-handle-set-background)
				     ("-geometry" . amiga-handle-set-geometry)
				     ("-screen" . amiga-handle-set-screen))
				   command-switch-alist))

(defun amiga-handle-set-font (switch)
  (condition-case err
      (let ((wfont (car command-line-args-left))
	    (height (car (read-from-string (car (cdr command-line-args-left))))))
	(setq command-line-args-left (cdr (cdr command-line-args-left)))
	(amiga-set-font wfont height))
    (error (message "Failed to load font"))))

(defun amiga-handle-set-foreground (switch)
  (condition-case err
      (let ((pen (car (read-from-string (car command-line-args-left)))))
	(setq command-line-args-left (cdr command-line-args-left))
	(amiga-set-foreground-color pen))
    (error (message "Failed to set foreground colour"))))

(defun amiga-handle-set-background (switch)
  (condition-case err
      (let ((pen (car (read-from-string (car command-line-args-left)))))
	(setq command-line-args-left (cdr command-line-args-left))
	(amiga-set-background-color pen))
    (error (message "Failed to set background colour"))))

(defun amiga-handle-set-screen (switch)
  (condition-case err
      (let ((name (car command-line-args-left)))
	(setq command-line-args-left (cdr command-line-args-left))
	(amiga-set-geometry nil nil nil nil name))
    (error (message "Couldn't open on public screen"))))

(defun amiga-handle-set-geometry (switch)
  (condition-case err
      (let ((x (car (read-from-string (car command-line-args-left))))
	    (y (car (read-from-string (car (cdr command-line-args-left)))))
	    (w (car (read-from-string (car (cdr (cdr command-line-args-left))))))
	    (h (car (read-from-string (car (cdr (cdr (cdr command-line-args-left)))))))
)
	(setq command-line-args-left (cdr (cdr (cdr (cdr command-line-args-left)))))
	(amiga-set-geometry x y w h nil))
    (error (message "Couldn't set window size"))))

;;
;;      A new icon-creating scheme.
;;  When creating icons, the following replacement routine searches
;;  for icons of the name "def_<extension>.info".
;;  The routine searches first in the directories specified by the
;;  user in the variable "amiga-icon-path" and then in "env:Icons/".
;;  The directories in the list must contain the trailing slash.
;;
;;  If it can't find any appropriate icons, and the file
;;  "env:Icons/def_emacs.info" exists, it is used. Otherwise
;;  the original function is called.
;;
;;  Note that icons are only created when the variable
;;  "amiga-create-icons" is non-nil.
;;
;;  Example: If the user would like to use the icons supplied by
;;           the SAS C-complier, the following lines could be placed
;;           in his or hers .emacs file:
;;      (setq amiga-icon-path '("sc:Icons/"))
;;      (setq amiga-create-icons t)
;;

(defvar amiga-icon-path '()
  "A list of directories to scan when searching for new icons.")

(if (not (fboundp 'old-amiga-put-icon))
    (fset 'old-amiga-put-icon (symbol-function 'amiga-put-icon)))

(defun amiga-put-icon (file force)
  (if (or force (not (file-readable-p (concat file ".info"))))
      (let ((extpos (string-match "\\.[a-zA-Z]\\'" file))
            (iconname nil)
            (path (append amiga-icon-path '("env:icons/")))
            (found nil))
        (if (and extpos (< 0 extpos))
            (while (and (not found) path)
              (setq iconname (concat (car path) "def_" (substring file (+ 1 extpos)) ".in\
fo"))
              (if (and iconname (file-readable-p iconname))
                  (progn
                    (copy-file iconname (concat file ".info"))
                    (setq found t)))
              (setq path (cdr path))))
        (if (not found)
            (if (file-readable-p "env:Icons/def_emacs.info")
                (copy-file "env:Icons/def_emacs.info" (concat file ".info"))
              (old-amiga-put-icon file force))))))



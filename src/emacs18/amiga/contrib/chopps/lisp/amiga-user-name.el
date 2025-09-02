;;
;;  User-Name.el.
;;  ------------
;;   -ch3/11/93.
;;
;;  This is a replacement for the (user-full-name) function that comes
;;  comes with emacs, so that it fetches the login name from $USERNAME
;;  and the full name from $REALNAME.  As is the default on most amiga
;;  systems that are running UUCP and related utils.
;;
;;  It will also check $USER for the login name.
;;

(defun user-login-name ()
  (if (eq nil (getenv "UserName"))
      (getenv "User")
    (getenv "UserName")))

(defun user-full-name ()
  (if (eq nil (getenv "RealName"))
      (user-login-name)
    (getenv "RealName")))


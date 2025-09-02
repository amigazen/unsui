/* The default search path for Lisp function "load".
   This sets load-path.  */
#define PATH_LOADSEARCH "GNUEmacs:lisp"

/* the extra search path for programs to invoke.
 This is appended to whatever the PATH environment variable says
 to set the Lisp variable exec-path and the first file namein it
  sets the Lisp variable exec-directory.  */
#define PATH_EXEC "GNUEmacs:etc"

/* the name of the directory that contains lock files
 with which we record what files are being modified in Emacs.
 This directory should be writable by everyone.
 THE STRING MUST END WITH A SLASH!!!  */
#define PATH_LOCK "GNUEmacs:lock/"

/* the name of the file !!!SuperLock!!! in the directory
 specified by PATH_LOCK.  Yes, this is redundant.  */
#define PATH_SUPERLOCK "GNUEmacs:lock/!!!SuperLock!!!"

/* The path to the file containing the termcap descriptions */
#define PATH_TERMCAP "s:termcap"

/* The relative path (while dumping) to the directory containing 
   the DOC file */
#define RELPATH_DOC "/etc/"

/* The path for a /dev/null-like device */
#define PATH_NULL "NIL:"

/* Path for temporary files (for call-process-region) */
#define PATH_TEMP "t:emacsXXXXXX"

/* Path to the shell (the one in shell-file-name) */
#define PATH_SHELL "GNUEmacs:etc/sh"

/* sayval.rexx
 * -----------
 *  -ch3/18/93.
 *
 *  call from dos prompt:
 *
 *  sayval <some-emacs-symbol-name>
 *
 * ex:  sayval amiga-emacs-iconified
 * 
 *  Christian E. Hopps.
 */

/* get the first arg from the command line and store in symbol */
parse arg symbol

/* address the emacs port */
address EMACS1

/* ask for full results */
options results 

/* construct lisp inside emacs this will look like this
 *
 * (symbol-value 'some-variable-name) */

/* call function by quoting the lisp code. */
"(symbol-value '" || symbol || ")"
say RESULT

/*
 * This is a code fragment from a patched version of dospecial.c
 * dvips version 5.47
 *
 * The changed lines are between the TJD comments.
 * The patch allows for zcat input to plotfile commands.
 *
 */


   if (strncmp(p, "ps:", 3)==0) {
        hvpos() ;
        psflush() ; /* now anything can happen. */
        if (p[3]==':') {
           if (strncmp(p+4, "[begin]", 7) == 0)
              nlcmdout(&p[11]);
           else if (strncmp(p+4, "[end]", 5) == 0)
              nlcmdout(&p[9]);
           else nlcmdout(&p[4]);
        } else if (strncmp(p+3, " plotfile ", 10) == 0) {
           char *sfp ;
           p += 13;
	   /*
	    *  Fixed to allow popen input for plotfile
	    *  TJD 10/20/91
	    */
	   while (*p == ' ') p++;
	   if (*p == '"') {
	     p++;
	     for (sfp = p; *sfp && *sfp != '"'; sfp++) ;
           } else {
	     for (sfp = p; *sfp && *sfp != ' '; sfp++) ;
           }
           *sfp = '\0';
	   if (*p == '`') 
	     figcopyfile(p+1, 1);
	   else
	     figcopyfile (p, 0);
	   /* End TJD changes */
        } else
           nlcmdout(&p[3]);
        return;
   }

@x
extern char *figpath ;
@y
extern char *figpath ;
extern struct EnvVarPath *fig_var;
@z

@x
	FILE *f;
	char *env;
@y
	FILE *f;
@z

@x
	f = search(figpath, fname, READBIN);
	if (f == (FILE *)NULL) {
   	    if ( (env = getenv("DVIDRVGRAPH")) != NULL )
		f = search(env,filename,READBIN);
	}
@y
	f = search(fig_var, fname, READBIN);
@z

@x
		f = search(figpath, fname, READBIN);
		if (f == (FILE *)NULL) {
	    	    if ( (env = getenv("DVIDRVGRAPH")) != NULL )
			f = search(env,filename,READBIN);
		}
		if (f != (FILE *)NULL)
		    break;
		i++;
@y
		if ((f = search(fig_var, fname, READBIN)) != NULL)
			break;
		else
			i++;
@z

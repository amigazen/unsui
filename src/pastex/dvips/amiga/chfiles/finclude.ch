@x
extern char *figpath ;
@y
extern char *figpath ;
extern struct EnvVarPath *fig_var;
@z

@x
   f = search(figpath, filename, READ) ;
@y
   f = search(fig_var, filename, READ) ;
@z

@x
extern void fil2ps();
extern FILE *search();
extern int system();
@y
@z

@x
extern char *figpath ;
@y
extern char *figpath ;
extern struct EnvVarPath *fig_var;
@z

@x
   FILE *f = search(figpath, s, "r") ;
@y
   FILE *f = search(fig_var, s, "r") ;
@z

@x
extern char *pictpath ;
@y
extern char *pictpath ;
extern struct EnvVarPath *pict_var;
@z

@x
   if (0 != (f=search(pictpath, iname, "r"))) {
@y
   if (0 != (f=search(pict_var, iname, "r"))) {
@z

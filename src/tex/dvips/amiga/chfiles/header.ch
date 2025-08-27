@x
extern char *headerpath ;
@y
extern char *headerpath ;
extern struct EnvVarPath *header_var;
@z

@x
   FILE *f = search(headerpath, s, READ) ;
@y
   FILE *f = search(header_var, s, READ) ;
@z

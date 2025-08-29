@x
extern char *fliname ;
@y
extern char *fliname ;
extern struct EnvVarPath *fli_var;
@z

@x
         if ( (pkfile=search(flipath,name,READBIN)) != (FILE *)NULL ) {
@y
         if ( (pkfile=search(fli_var,name,READBIN)) != (FILE *)NULL ) {
@z

@x
                        if ( (pkfile=search(flipath,lib->name,READBIN)) == (FILE *)NULL ) {
@y
                        if ( (pkfile=search(fli_var,lib->name,READBIN)) == (FILE *)NULL ) {
@z

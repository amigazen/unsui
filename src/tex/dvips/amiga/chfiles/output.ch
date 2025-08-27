@x
extern char *headerpath ;
@y
extern char *headerpath ;
extern struct EnvVarPath *header_var;
@z

@x
extern char *figpath ;
@y
extern char *figpath ;
extern struct EnvVarPath *fig_var;
@z

@x
      f = search(figpath, s, READ) ;
      if (f == 0)
         f = search(headerpath, s, READ) ;
@y
      f = search(fig_var, s, READ) ;
      if (f == 0)
         f = search(header_var, s, READ) ;
@z

@x
      f = search(headerpath, s, READ) ;
@y
      f = search(header_var, s, READ) ;
@z


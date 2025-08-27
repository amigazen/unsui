@x
extern long bytesleft ;
extern quarterword *raster ;
extern char *vfpath ;
@y
extern long bytesleft ;
extern quarterword *raster ;
extern char *vfpath ;
extern struct EnvVarPath *vf_var;
@z

@x
vfopen(fd)
        register fontdesctype *fd ;
{
   register char *d, *n ;
@y
vfopen(fd)
        register fontdesctype *fd ;
{
   register char *d, *n ;
   struct EnvVarPath *dummy_var = NULL;
@z

@x
   if (*d==0)
      d = vfpath ;
#ifdef MVSXA   /* IBM: MVS/XA */
   (void)sprintf(name, "vf(%s)", n) ;
#else
   (void)sprintf(name, "%s.vf", n) ;
#endif
   if (0 != (vffile=search(d, name, READBIN)))
      return(1) ;
   return(0) ;
}
@y

   if (*d==0)
      dummy_var = vf_var;
   else
   {
      dummy_var = Alloc_EnvVarPath("", 256L);
      Init_EnvVarPath(dummy_var, d, ENVPATH_DEFSTR);
   }

   (void)sprintf(name, "%s.vf", n) ;

   if (0 != (vffile=search(dummy_var, name, READBIN))) {
      if (dummy_var != vf_var)
         Free_EnvVarPath(dummy_var);
      return(1) ;
   }

   if (dummy_var != vf_var)
      Free_EnvVarPath(dummy_var);

   return(0) ;
}
@z


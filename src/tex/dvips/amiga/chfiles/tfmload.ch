@x
extern char *tfmpath ;
@y
extern char *tfmpath ;
extern struct EnvVarPath *tfm_var;
@z

@x
void
tfmopen(fd)
        register fontdesctype *fd ;
{
   register char *d, *n ;

   d = fd->area ;
   n = fd->name ;
   if (*d==0)
      d = tfmpath ;
#ifdef MVSXA   /* IBM: MVS/XA */
   (void)sprintf(name, "tfm(%s)", n) ;
#else
   (void)sprintf(name, "%s.tfm", n) ;
#endif
   if ((tfmfile=search(d, name, READBIN))==NULL) {
      (void)sprintf(errbuf, "Can't open font metric file %s%s",
             fd->area, name) ;
      error(errbuf) ;
      error("I will use cmr10.tfm instead, so expect bad output.") ;
#ifdef MVSXA   /* IBM: MVS/XA */
      if ((tfmfile=search(d, "tfm(cmr10)", READBIN))==NULL)
#else
      if ((tfmfile=search(d, "cmr10.tfm", READBIN))==NULL)
#endif
         error(
          "! I can't find cmr10.tfm; please reinstall me with proper paths") ;
   }
}
@y
void
tfmopen(fd)
        register fontdesctype *fd ;
{
   register char *d, *n ;
   struct EnvVarPath *dummy_var = NULL;

   d = fd->area ;
   n = fd->name ;

   if (*d==0)
   	dummy_var = tfm_var;
   else
   {
      dummy_var = Alloc_EnvVarPath("", 256L);
      Init_EnvVarPath(dummy_var, d, ENVPATH_DEFSTR);
   }

   (void)sprintf(name, "%s.tfm", n) ;

   if ((tfmfile=search(dummy_var, name, READBIN))==NULL) {
      (void)sprintf(errbuf, "Can't open font metric file %s%s",
             fd->area, name) ;
      error(errbuf) ;
      error("I will use cmr10.tfm instead, so expect bad output.") ;
      if ((tfmfile=search(dummy_var, "cmr10.tfm", READBIN))==NULL) {
         if (dummy_var != tfm_var)
	    Free_EnvVarPath(dummy_var);
         error("! I can't find cmr10.tfm; please reinstall me with proper paths") ;
      }
   }

   if (dummy_var != tfm_var)
	Free_EnvVarPath(dummy_var);
}
@z

@x
#ifdef AMIGA
  return(shalfword) (getc(tfmfile)) ;
#else
  return(getc(tfmfile)) ;
#endif
@y
  return(shalfword) (getc(tfmfile)) ;
@z

@x
#ifdef AMIGA
  return (halfword)( a * 256 + tfmbyte () ) ; 
#else
  return ( a * 256 + tfmbyte () ) ; 
#endif
@y
  return (halfword)( a * 256 + tfmbyte () ) ; 
@z

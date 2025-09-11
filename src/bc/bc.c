/*
 * bc - POSIX calculator
 * Main source module
 *
 * POSIX bc calculator implementation based on Eval 1.13
 *
 * Copyright (C) 1993  Will Menninger (original Eval)
 * Copyright (C) 2025 amigazen project
 *
 * SPDX-License-Identifier: GPL-2.0-or-later
 *
** To add a new constant to the Eval program
** -----------------------------------------
** 1.  Update the size of MAXC in eval.h
** 2.  Add your constant to the clist[] array below.  Please keep the
**     list in alphabetic order, and make sure that your contant's
**     name does not exceed MAXNAMELEN (from eval.h)
** 3.  If your constant is a physical constant with no indication
**     of units in the name, either make sure it is in SI units, or change
**     the print statement that claims all constants are in SI units.
** 4.  Recompile ALL source modules.
**
**
** Originally written 5/89 in ANSI C
**
*/

#include   "eval.h"

/* External variables for getopt */
extern int optind;
extern int getopt(int argc, char * const argv[], const char *optstring);

static char    tempname[80];
static char    wdir[100];
static char    rpath[200];
static FILE   *tempfile;
static int     linecount;

/* POSIX bc specific constants */
#define BC_VERSION "2.0"

static const char *verstag = "$VER: bc 2.0 (12/09/25)\n";

static const char *stack_cookie = "$STACK: 8192";

static VAR     clist[MAXC]=  {
                {"_acres_per_sq_km",247.1},
                {"_air_density",1.293},
                {"_air_mol_mass",.02897},
                {"_atm_per_psi",.06804},
                {"_avagadro",6.0220e23},
                {"_boltzmann",1.3807e-23},
                {"_c",2.997925e8},
                {"_cm_per_in",2.54},
                {"_coulomb_const",8.98755e9},
                {"_deg_per_rad",57.2958},
                {"_e",2.71828182845904523536},
                {"_earth_esc_spd",1.12e4},
                {"_earth_grav",9.80665},
                {"_earth_mass",5.98e24},
                {"_earth_radius",6.37e6},
                {"_earth_to_moon",3.844e8},
                {"_earth_to_sun",1.496e11},
                {"_eps0",8.85419e-12},
                {"_erg_per_joule",1e7},
                {"_eulers_const",.57721566490153286061},
                {"_ft_per_m",3.280839895},
                {"_g",6.672e-11},
                {"_gas_const",8.314},
                {"_gauss_per_tesla",1e4},
                {"_gm_per_oz",28.34952313},
                {"_golden_ratio",1.6180339887498948482},
                {"_h",6.6262e-34},
                {"_hbar",1.05459e-34},
                {"_joule_per_btu",1054.35},
                {"_joule_per_cal",4.184},
                {"_joule_per_ftlb",1.356},
                {"_joule_per_kwh",3.6e6},
                {"_kg_per_slug",14.59},
                {"_km_per_mi",1.609344},
                {"_knots_per_mph",.86897624},
                {"_lbs_per_kg",2.204622622},
                {"_lit_per_gal",3.785411784},
                {"_me",9.1095e-31},
                {"_mn",1.67495e-27},
                {"_moon_grav",1.62},
                {"_moon_mass",7.35e22},
                {"_moon_period",2360448.},
                {"_moon_radius",1.738e6},
                {"_mp",1.67265e-27},
                {"_mu0",1.256637e-6},
                {"_oz_per_gal",128.},
                {"_pasc_per_atm",101325.},
                {"_pasc_per_psi",6895.},
                {"_pasc_per_torr",133.32},
                {"_pi",3.14159265358979323846},
                {"_qe",1.60219e-19},
                {"_solar_const",1350.},
                {"_speed_sound",331.},
                {"_sun_mass",1.99e30},
                {"_sun_radius",6.96e8},
                {"_watts_per_hp",745.712},
                {"_zero_deg_cels",273.15}
                     };

static char *license =
    "Eval, a floating point expression evaluator, version %s\n"
    "Copyright (C) 1993  Will Menninger\n"
    "This program is freely redistributable under certain restrictions.\n"
    "It comes with absolutely no warranty.\n"
    "Type '\x3f\x3f' for more information, or '?' for brief help.\n";


static int process_file(char *filename,int bequiet,VARPTR vlist,VARPTR clist);
static BOOLEAN process_line(FILE *stream,int showinp,int showout,VARPTR vlist,
                            VARPTR clist,char *pinput);
static void init_varlist(VARPTR vlist);
static void var_copy(VARPTR dest,VARPTR source);
static int print_help(FILE *stream,int extended,int page,char *s);
static int more(char *text,char *input,int pause);
static void print_oplist(void);
static int  print_varlist(FILE *s,char *input,VARPTR list,int max);
static void print_stats(void);
static int nextline(char *s,FILE *stream);
static void close_temp(int showout);
static int srchpath(char *name);
static void cwdir(char *name);
static int parse_in_current_base(char *s);


int main(int argc,char *argv[])

    {
    VAR     vlist[MAXV];
    int     opt, i;
    FILE    *input_file;
    BOOLEAN show_help = FALSE;
    BOOLEAN show_version = FALSE;
    BOOLEAN quiet = FALSE;

    /* Parse command line arguments */
    while ((opt = getopt(argc, argv, "hvq")) != -1) {
        switch (opt) {
            case 'h':
                show_help = TRUE;
                break;
            case 'v':
                show_version = TRUE;
                break;
            case 'q':
                quiet = TRUE;
                break;
            default:
                fprintf(stderr, "Usage: bc [-h] [-v] [-q] [file...]\n");
                return 1;
        }
    }

    /* Handle help and version options */
    if (show_help) {
        printf("bc - POSIX calculator\n");
        printf("Options:\n");
        printf("  -h, --help     Show this help message\n");
        printf("  -v, --version  Show version information\n");
        printf("  -q, --quiet    Suppress welcome message\n");
        printf("  file           Read commands from file(s)\n");
        printf("                 If no files specified, read from stdin\n");
        return 0;
    }
    if (show_version) {
        printf("bc %s - POSIX calculator for Amiga\n", BC_VERSION);
        return 0;
    }

    /* Initialize calculator */
    init_varlist(vlist);
    wdir[0]=EOS;
    rpath[0]=EOS;
    setobase(10);
    setibase(10);
    set_scinote(0);
    set_fix(0);
    set_sigfig(10);
    set_dplace(10);
    set_maxexp(5);
    tempfile=NULL;

    /* Process input files or stdin */
    if (optind < argc) {
        /* Process files specified on command line */
        for (i = optind; i < argc; i++) {
            input_file = fopen(argv[i], "r");
            if (input_file == NULL) {
                fprintf(stderr, "bc: cannot open %s\n", argv[i]);
                return 1;
            }
            while (!process_line(input_file, 0, 1, vlist, clist, ""));
            fclose(input_file);
        }
    } else {
        /* Process stdin */
        while (!process_line(stdin, 1, 1, vlist, clist, ""));
    }

    close_temp(1);
    return 0;
    }


/*
** process_file
**
** Sends all of the lines from file, filename, to Eval for processing.
**
** Returns 1 if a quit was encountered in the file, 0 otherwise
**
*/

static int process_file(char *filename,int bequiet,VARPTR vlist,VARPTR clist)

    {
    FILE   *s;
    int     status;

    s=fopen(filename,"r");
    if (s==NULL)
        {
        printf("File %s not found.\n",filename);
        return(0);
        }
    while (1)
        {
        status=process_line(s,!bequiet,!bequiet,vlist,clist,"");
        if (status==1 || status==2)
            break;
        }
    fclose(s);
    return(status==1 ? 1 : 0);
    }


/*
** process_line
**
** Processes one line of input from stream.
** If showinp is NZ, the input is printed to stdout.
** If showout is NZ, all output is printed to stdout.
**
** If stream==NULL, input is taken from string: pinput
**
** Returns:   1 if "quit" (or similar command) is read.
**            2 if end of file
**            0 otherwise.
**
*/

static BOOLEAN process_line(FILE *stream,int showinp,int showout,VARPTR vlist,
                            VARPTR clist,char *pinput)

    {
    char    rname[100];
    char    input[MAXINPUT+1];
    int     bequiet,i,m0,m1,m2,n,nargs;

    input[0]=EOS;
    if (stream!=NULL)
        {
        if (!nextline(input,stream))
            return(2);
        }
    else
        strcpy(input,pinput);

    while (1)
        {
        if (input[0]=='@')
            {
            bequiet=1;
            for (i=1;input[i]==' ' || input[i]=='\t';i++);
            }
        else
            {
            bequiet=0;
            i=0;
            }
        if (input[i]!='<' && input[i]!='>' && tempfile!=NULL)
            {
            fprintf(tempfile,"%s\n",&input[i]);
            linecount++;
            }
        if (showinp && !bequiet && input[i]!=EOS)
            printf("%s%s\n", PROMPT, input);
        /* POSIX bc quit commands */
        if (!strcmp(&input[i],"quit") || !strcmp(&input[i],"exit") ||
            !strcmp(&input[i],"stop") || !strcmp(&input[i],"end"))
            return(1);
        if (input[i]==EOS)
            {
            if (!showinp && !bequiet && showout)
                printf("Type '?' or 'help' for help.\n");
            return(0);
            }
        if (!strncmp(&input[i],"help",4) &&
              (input[i+4]==' ' || input[i+4]=='\t' || input[i+4]==EOS))
            {
            if (input[i+4]!=EOS)
                n=atoi(&input[i+4]);
            else
                n=0;
            if (showout && !bequiet && print_help(stream,1,n,input))
                continue;
            else
                break;
            }
        
        /* POSIX bc specific commands */
        if (!strncmp(&input[i],"scale",5) &&
              (input[i+5]==' ' || input[i+5]=='\t' || input[i+5]==EOS))
            {
            if (input[i+5]!=EOS)
                {
                n=parse_in_current_base(&input[i+5]);
                if (n>=0 && n<=20)  /* POSIX allows scale 0-20 */
                    {
                    set_sigfig(n);
                    set_dplace(n);
                    if (showout && !bequiet)
                        printf("scale = %d\n", n);
                    }
                else
                    {
                    if (showout && !bequiet)
                        printf("scale must be between 0 and 20\n");
                    }
                }
            else
                {
                if (showout && !bequiet)
                    printf("scale = %d\n", get_sigfig());
                }
            break;
            }
        if (!strncmp(&input[i],"ibase",5) &&
              (input[i+5]==' ' || input[i+5]=='\t' || input[i+5]==EOS))
            {
            if (input[i+5]!=EOS)
                {
                n=parse_in_current_base(&input[i+5]);
                if (n>=2 && n<=16)  /* POSIX allows ibase 2-16 */
                    {
                    setibase(n);
                    if (showout && !bequiet)
                        printf("ibase = %d\n", n);
                    }
                else
                    {
                    if (showout && !bequiet)
                        printf("ibase must be between 2 and 16\n");
                    }
                }
            else
                {
                if (showout && !bequiet)
                    printf("ibase = %d\n", getibase());
                }
            break;
            }
        if (!strncmp(&input[i],"obase",5) &&
              (input[i+5]==' ' || input[i+5]=='\t' || input[i+5]==EOS))
            {
            if (input[i+5]!=EOS)
                {
                n=parse_in_current_base(&input[i+5]);
                if (n>=2 && n<=16)  /* POSIX allows obase 2-16 */
                    {
                    setobase(n);
                    if (showout && !bequiet)
                        printf("obase = %d\n", n);
                    }
                else
                    {
                    if (showout && !bequiet)
                        printf("obase must be between 2 and 16\n");
                    }
                }
            else
                {
                if (showout && !bequiet)
                    printf("obase = %d\n", getobase());
                }
            break;
            }
        if (input[i]=='<' || input[i]=='>')
            {
            n=input[i];
            for (i++;input[i]==' ' || input[i]=='\t';i++);
            if (n=='<')
                {
                if (input[i]==EOS)
                    {
                    printf("Must specify a file name after <.\n");
                    return(0);
                    }
                strcpy(rname,&input[i]);
                if (!srchpath(rname))
                    {
                    printf("Cannot find %s in read path.\n",rname);
                    return(0);
                    }
                return(process_file(rname,bequiet || !showout,vlist,clist));
                }
            close_temp(showout && !bequiet);
            if (input[i]!=EOS)
                {
                strcpy(tempname,&input[i]);
                cwdir(tempname);
                tempfile=fopen(tempname,"w");
                linecount=0;
                if (tempfile==NULL)
                    printf("Cannot open %s for output.\n",tempname);
                else
                    if (showout && !bequiet)
                        printf("Script file %s begun.\n",tempname);
                }
            return(0);
            }
        nargs=1;
        m0=i;
        showout=(showout && !bequiet);
        while (1)
            {
            for (;!isspace(input[i]) && input[i]!=EOS;i++);
            if (nargs==1)
                m1=i-m0;
            if (input[i]==EOS)
                break;
            for (;isspace(input[i]);i++);
            if (input[i]==EOS)
                break;
            nargs++;
            if (nargs==2)
                m2=i;
            if (nargs>2)
                break;
            }
        if (nargs==2)
            n=atoi(&input[m2]);
        if (!strncmp(&input[m0],"rpath",m1))
            {
            if (nargs==2)
                {
                strcpy(rpath,&input[m2]);
                if (showout)
                    printf("Read path set to %s\n",rpath);
                }
            else
                {
                rpath[0]=EOS;
                if (showout)
                    printf("Read path cleared.\n");
                }
            break;
            }
        if (!strncmp(&input[m0],"wdir",m1))
            {
            if (nargs==2)
                {
                strcpy(wdir,&input[m2]);
                if (showout)
                    printf("Write dir set to %s\n",wdir);
                }
            else
                {
                wdir[0]=EOS;
                if (showout)
                    printf("Write dir set to current dir.\n");
                }
            break;
            }
        if (!strncmp(&input[m0],"obase",m1) && nargs==2 && n>0)
            {
            if (n<2 || n>36)
                {
                printf("Output base must be between 2 and 36.\n");
                break;
                }
            setobase(n);
            if (showout)
                printf("Output base set to %d.\n",n);
            break;
            }
        if (!strncmp(&input[m0],"ibase",m1) && nargs==2 && n>0)
            {
            if (n<2 || n>36)
                {
                printf("Input base must be between 2 and 36.\n");
                break;
                }
            setibase(n);
            if (showout)
                printf("Input base set to %d.\n",n);
            break;
            }
        if (!strncmp(&input[m0],"sci",m1) && nargs<2)
            {
            set_scinote(!get_scinote());
            if (showout)
                printf("Scientific notation %s.\n",get_scinote() ? "ON":"OFF");
            break;
            }
        if (!strncmp(&input[m0],"fix",m1) && nargs<2)
            {
            set_fix(!get_fix());
            if (showout)
                printf("Fixed precision %s.\n",get_fix() ? "ON":"OFF");
            break;
            }
        if (!strncmp(&input[m0],"sigfig",m1) && nargs==2)
            {
            if (n>DBL_MANT_DIG)
                printf("Significant figures limited to %d.\n",
                        DBL_MANT_DIG);
            else
                {
                set_sigfig(n);
                if (showout)
                    {
                    printf("Significant figures = ");
                    if (n>0)
                        printf("%d\n",n);
                    else
                        printf("max\n");
                    }
                }
            break;
            }
        if (!strncmp(&input[m0],"maxexp",m1) && nargs==2)
            {
            set_maxexp(n);
            if (showout)
                {
                if (n<0)
                    printf("Exponent limit turned off.\n");
                else
                    printf("Exponent limit = %d\n",n);
                }
            break;
            }
        if (!strncmp(&input[m0],"dplace",m1) && nargs==2)
            {
            if (n<-70 || n>70)
                printf("Decimal place must be between -70 and 70.\n");
            else
                {
                set_dplace(n);
                if (showout)
                    printf("Decimal place = %d\n",n);
                }
            break;
            }
        if (input[m0]!='?')
            {
            evaluate(&input[m0],showout,vlist,clist);
            break;
            }
        if (!strcmp(&input[m0],"?"))
            {
            if (showout && print_help(stream,0,0,input))
                continue;
            else
                break;
            }
        for (i=m0+1;isspace(input[i]);i++);
        if (input[i]=='?')
            {
            if (input[i+1]!=EOS)
                n=atoi(&input[i+1]);
            else
                n=0;
            if (showout && print_help(stream,1,n,input))
                continue;
            else
                break;
            }
        switch (tolower((int)input[i]))
            {
            case 'v':
                if (showout && print_varlist(stream,input,vlist,MAXV))
                    continue;
                break;
            case 'c':
                if (showout)
                    {
                    printf("All physical constants are in SI units unless the "
                           "name of the variable\nindicates otherwise.\n");
                    if (print_varlist(stream,input,clist,MAXC))
                        continue;
                    }
                break;
            case 'f':
                if (showout)
                if (print_funclist(stream,input,tolower((int)input[i+1])=='l'))
                    continue;
                break;
            case 'o':
                if (showout)
                    print_oplist();
                break;
            case 's':
                if (showout)
                    print_stats();
                break;
            default:
                printf("\"%s\" is an unknown query.\n",&input[i]);
                break;
            }
        break;
        }
    return(0);
    }


/*
** insert_var(VARPTR new,VARPTR vlist)
**
** Inserts a the new variable into the current list of variables.
** Returns 1 if successful, 0 if list is full.
**
*/

BOOLEAN insert_var(VARPTR new,VARPTR vlist)

   {
   BOOLEAN found;
   int     i,j,k;

   found=search_varlist(new,vlist,&i,MAXV);
   if (!found && vlist[MAXV-1].name[0]!=EOS)
       return(0);
   if (found)
       var_copy(&vlist[i],new);
   else
       {
       for (j=i;j<MAXV-1 && vlist[j].name[0]!=EOS;j++);
       for (k=j;k>i;k--)
           var_copy(&vlist[k],&vlist[k-1]);
       var_copy(&vlist[i],new);
       }
   return(1);
   }


/*
** init_varlist(VARPTR vlist)
**
** Sets all variable names to EOS, representing empty spaces
**
*/

static void init_varlist(VARPTR vlist)

   {
   int     i;

   for (i=0;i<MAXV;i++)
       vlist[i].name[0]=EOS;
   }

/*
** var_copy(VARPTR dest,VARPTR source)
**
** copies one variable to another
**
*/

static void var_copy(VARPTR dest,VARPTR source)

   {
   strcpy(dest->name,source->name);
   dest->value=source->value;
   }


/*
** search_varlist(VARPTR var,VARPTR vlist,int *n,int max)
**
** Searches through a variable list for the variable specified.  If
** found, returns 1 else returns 0.  The position for insertion is
** returned in (*n).  The search is binary for fast response.
**
*/

BOOLEAN search_varlist(VARPTR var,VARPTR vlist,int *n,int max)

   {
   int     step,c;

   (*n)=step=max>>1;
   while (1)
       {
       if (step>1)
           step>>=1;
       if (vlist[(*n)].name[0]==EOS)
           {
           if ((*n)==0)
               return(0);
           (*n)=(*n)-step;
           continue;
           }
       if (!(c=strcmp(var->name,vlist[(*n)].name)))
           break;
       if (c>0)
           {
           if ((*n)==max-2 || vlist[(*n)+1].name[0]==EOS)
               {
               (*n)=(*n)+1;
               if (vlist[(*n)].name[0]!=EOS)
                   return(!strcmp(var->name,vlist[(*n)].name));
               else
                   return(0);
               }
           (*n)=(*n)+step;
           continue;
           }
       if ((*n)==0 || strcmp(var->name,vlist[(*n)-1].name)>0)
           return(0);
       (*n)=(*n)-step;
       }
   return(1);
   }


/*
** print_help(void)
**
** Prints instructions on how to use the expression evaluator.
**
*/

static int print_help(FILE *stream,int extended,int page,char *s)

    {
    extern char *bhelp;
    extern char *ehelp;
    int     i,j;

    if (extended)
        for (i=0,j=0;ehelp[j]!=EOS && i<page-1;j++)
            if (ehelp[j]=='\f')
                {
                j++;
                i++;
                }

    return (more(extended ? &ehelp[j] : bhelp,s,stream==stdin));
    }


static int more(char *text,char *input,int pause)

    {
    int     nl;
    int     c,in,istart;

    in=-1;
    while (1)
        {
        istart=in+1;
        for (nl=0;nl<NLINES;nl++)
            {
            for (in++;text[in]!='\f' && text[in]!='\n' && text[in]!=EOS;in++);
            if (text[in]==EOS || text[in]=='\f')
                break;
            }
        if (text[in]==EOS || text[in+1]==EOS)
            {
            printf("%s\n",&text[istart]);
            break;
            }
        if (text[in]=='\f')
            in--;
        c=text[in+1];
        text[in+1]=EOS;
        printf("%s\n",&text[istart]);
        text[in+1]=c;
        if (pause)
            {
            printf("Press <ENTER> for more...\n");
            input[0]=EOS;
            nextline(input,stdin);
            if (input[0]!=EOS)
                return(1);
            printf("\n");
            }
        if (c=='\f')
            in+=2;
        }
    return(0);
    }


/*
** print_oplist(void)
**
** Prints list of operands
**
*/

static void print_oplist(void)

    {
    static char *oplist =
    "\nUnary operators:\n"
    "                  +      positive\n"
    "                  -      negative\n"
    "                  ~      bit-wise NOT\n\n"
    "Binary operators: (in order of precedence)\n"
    "                  ^      power*\n"
    "                  * / %  multiply, divide, modulo\n"
    "                  + -    add, subtract\n"
    "                  << >>  bit-shift left, right\n"
    "                  &      bit-wise AND\n"
    "                  ?      bit-wise XOR*\n"
    "                  |      bit-wise OR\n\n"
    "* - different from C\n\n";
    printf("%s",oplist);
    }


/*
** print_varlist(FILE *s,char *input,VARPTR list,int max)
**
** Prints out a list of constants or variables and their values
**
*/

static int print_varlist(FILE *s,char *input,VARPTR list,int max)

    {
    char    bigbuf[MAXOUTLEN];
    char    bigline[MAXOUTLEN];
    int     i,j,k,l,n,m1,m2,nc,cw,nr,li,c;

    for (m1=m2=0,i=0;i<max && list[i].name[0]!=EOS;i++)
        {
        if ((l=strlen(list[i].name))>m1)
            m1=l;
        baseconv(list[i].value,bigbuf);
        if ((l=strlen(bigbuf))>m2)
            m2=l;
        }
    n=i;
    if (!n)
        {
        printf("There are no currently assigned variables.\n");
        return(0);
        }
    cw=m1+m2+7;
    nc=(SCRWIDTH+3)/cw;
    if (nc<=0)
        nc=1;
    nr=(i+nc-1)/nc;
    for (i=0;i<nr;i++)
        {
        li=0;
        for (j=0;j<nc;j++)
            {
            k=i+j*nr;
            if (k>=n)
                break;
            for (l=0;(c=list[k].name[l])!=EOS;l++)
                bigline[li++]=c;
            for (;l<m1;l++)
                bigline[li++]=' ';
            baseconv(list[k].value,bigbuf);
            bigline[li++]=' ';
            bigline[li++]='=';
            bigline[li++]=' ';
            for (l=0;(c=bigbuf[l])!=EOS;l++)
                bigline[li++]=c;
            if (j<nc-1)
                for (;l<m2+4;l++)
                    bigline[li++]=' ';
            }
        bigline[li]=EOS;
        printf("%s\n",bigline);
        if (s==stdin && i!=nr-1 && ((i+3)%NLINES)==0)
            {
            printf("\nPress <ENTER> for more...\n");
            input[0]=EOS;
            nextline(input,s);
            if (input[0]!=EOS)
                return(1);
            printf("\n");
            }
        }
    printf("\n");
    return(0);
    }


static void print_stats(void)

    {
    int     ib,ob;

    ib=getibase();
    ob=getobase();
    printf("\nExpression evaluator, version %s\n",VERSION);
    print_outtype();
    printf("Default input base:   %d\n",ib);
    printf("Default output base:  %d\n\n",ob);
    printf("Base %d accuracy:     %d digits\n",ib,precision(ib));
    if (ob!=ib)
        printf("Base %d accuracy:     %d digits\n",ob,precision(ob));
    printf("\n");
    printf("Read path:  %s\n",rpath[0]==EOS ? "None" : rpath);
    printf("Write dir:  %s\n",wdir[0]==EOS ? "Current dir" : wdir);
    printf("\n");
    }


/*
** void fixup(char *s)
**
** Removes white space from beginning and end of string.  Also removes
** \n from end of string, and converts entire string to lower case.
**
*/

void fixup(char *s)

    {
    int     i,j;

    for (i=0;isspace(s[i]);i++);
    for (j=0;s[i]!='\n' && s[i]!=EOS;j++,i++)
        s[j]=tolower((int)s[i]);
    for (j--;j>=0 && isspace(s[j]);j--);
    s[j+1]=EOS;
    }


static int nextline(char *s,FILE *stream)

    {
    while (1)

        {
        if (stream==stdin)
            printf("%s",PROMPT);
        if (fgets(s,MAXINPUT,stream)==NULL)
            return(0);
        fixup(s);
        if (s[0]!=';')
            break;
        }
    return(1);
    }


static void close_temp(int showout)

    {
    if (tempfile!=NULL)
        {
        fclose(tempfile);
        tempfile=NULL;
        if (showout)
            printf("%d lines written to %s.\n",linecount,tempname);
        }
    }


static int srchpath(char *name)

    {
    char    buf[100];
    int     i,j;
    int     checked;
    FILE   *f;

    i=0;
    strcpy(buf,name);
    checked=0;
    while (1)
        {
        for (j=0;rpath[i]!=EOS && rpath[i]!=';';i++,j++)
            buf[j]=rpath[i];
        buf[j]=EOS;
        if (checked && !j && rpath[i]==EOS)
            return(0);
        while (rpath[i]==';')
            i++;
        strcat(buf,name);
        if (j || !checked)
            {
            if (!j)
                checked=1;
            f=fopen(buf,"r");
            if (f!=NULL)
                break;
            }
        }
    fclose(f);
    strcpy(name,buf);
    return(1);
    }


static void cwdir(char *name)

    {
    char    buf[100];
    FILE   *f;

    strcpy(buf,wdir);
    strcat(buf,name);
    f=fopen(buf,"w");
    if (f==NULL)
        return;
    fclose(f);
    strcpy(name,buf);
    }


static int parse_in_current_base(char *s)

    {
    int     base, val, i;
    char    c;

    base = getibase();
    val = 0;
    for (i = 0; s[i] != EOS; i++)
        {
        c = tolower((int)s[i]);
        if (c >= '0' && c <= '9')
            val = val * base + (c - '0');
        else if (c >= 'a' && c <= 'f')
            val = val * base + (c - 'a' + 10);
        else
            break;
        }
    return val;
    }

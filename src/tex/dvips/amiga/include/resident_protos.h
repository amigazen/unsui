/* Prototypes for functions defined in
resident.c
 */

int hash(char * s);

void revpslists(void);

void cleanres(void);

struct resfont * lookup(char * name);

void add_entry(char * TeXname,
               char * PSname,
               char * specinfo,
               char * downloadinfo);

int residentfont(register fontdesctype * curfnt);

void bad_config(void);

char * configstring(char * s,
                    int nullok);

void getdefaults(char * s);

void getpsinfo(char * name);

void checkenv(int which);

char * xmalloc(unsigned int size);

char * xrealloc(char * ptr,
                unsigned int size);

int is_dir(char * fn);

char * do_subdir_path(char * dir_list);


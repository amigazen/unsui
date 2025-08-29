/* Prototypes for functions defined in
finclude.c
 */

fontdesctype * ifontdef(char * name,
                        char * area,
                        integer scsize,
                        integer dssize,
                        char * scname);

void setfamily(fontdesctype * f);

char * getname(char * s);

void includechars(fontdesctype * f,
                  char * s);

void scan1fontcomment(char * p);

integer scanvm(char * p);

void scanfontcomments(char * filename);

Boolean okascmd(char * ss);

void nameout(char * area,
             char * name);

void fonttableout(void);


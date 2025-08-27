/* Prototypes for functions defined in
search.c
 */

#include <evpaths.h>

FILE * search(struct EnvVarPath * env_var,
              char * file,
              char * mode);

FILE * pksearch(char * path,
                char * file,
                char * mode,
                char * n,
                halfword dpi,
                halfword vdpi);

FILE * my_real_fopen(register char * n,
                     register char * t);


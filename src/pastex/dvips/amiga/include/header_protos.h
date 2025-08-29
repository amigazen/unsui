/* Prototypes for functions defined in
header.c
 */

int add_name(char * s,
             struct header_list ** what);

void checkhmem(char * s);

int add_header(char * s);

char * get_name(struct header_list ** what);

void send_headers(void);


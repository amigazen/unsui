/* Prototypes for functions defined in
color.c
 */

void colorcmdout(char * s);

void initcolor(void);

void background(char * bkgrnd);

void pushcolor(char * p,
               Boolean outtops);

void popcolor(Boolean outtops);

void resetcolorstack(char * p,
                     int outtops);

void bopcolor(int outtops);


/* Prototypes for functions defined in
unpack.c
 */

shalfword getnyb(void);

Boolean getbit(void);

long pkpackednum(void);

void flip(register char * s,
          register long howmany);

long unpack(quarterword * pack,
            halfword * raster,
            halfword cwidth,
            halfword cheight,
            halfword cmd);


/* Prototypes for functions defined in
drawPS.c
 */

void setPenSize(char * cp);

void addPath(char * cp);

void arc(char * cp,
         int invis);

void flushDashedPath(int dotted,
                     double inchesPerDash);

void flushPath(int invis);

void flushDashed(char * cp,
                 int dotted);

void flushSpline(char * cp);

void SetShade(register char * cp);

void shadeLast(char * cp);

void whitenLast(void);

void blackenLast(void);


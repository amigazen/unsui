/*
   sc_rexx.h: SAS/C rexx interface

   bf 11-21-96
*/

int init_scmsg (void);
void rexx_cleanup (void);
char *send_rexx_msg (char *cmd);
void clear_rexx_result (void);

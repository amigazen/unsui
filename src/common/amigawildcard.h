
typedef struct
{
   char *str;
}
t_str;

typedef struct
{
   t_str *string;
   int len;
   int len_alloc;
}
t_strlist;

extern int scan_pattern(char *source,t_strlist *strl);
extern void init_list(t_strlist *strl);
extern void clear_list(t_strlist *strl);
extern void print_list(t_strlist *strl);
extern char *pop_elt(int index,t_strlist *strl);
extern void fill_list(char **argv,int argstart,int argcount,t_strlist *strl);

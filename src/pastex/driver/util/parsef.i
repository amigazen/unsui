void parse_file(struct string_parse *parse, FILE *file,int parse_control);

#ifdef DISPLAY
extern int  *parse_ints(char *line);
#endif
extern char *parse_bytes(char *line);

extern char *args_line(char *line);
extern char *split_line(char *line, char *separators);
extern char *nextpos_line(char *line, char *lastpos);
extern char *endpos_line(char *line);
extern unsigned fields_line(char *line, char *lastpos);

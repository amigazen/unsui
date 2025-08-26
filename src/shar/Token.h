typedef enum
{
     T_CAT,    /* The token is the "cat" command.  To be a command the token
                  must be the first one on the line. */
     T_ECHO,   /* The token is the "echo" command.  It must be the first
                  token on the line. */
     T_GT,     /* The token is ">" indicating redirection of standard
                  output.  It must be preceded by a T_CAT or T_ECHO
                  token. */
     T_LTLT,   /* The token is "<<" indicating "here document" redirection.
                  It must be preceded by a T_CAT or T_ECHO token.  The next
                  word defines the "here document" terminator.  Subsequent
                  lines from the input file are treated as input for the
                  command on this line. */
     T_NL,     /* The token is a newline character.  It means we have
                  encountered the end of the text line.  There are no more
                  tokens to identify. */
     T_WORD    /* The token is any word not of the above type.  A word is a
                  string of characters surrounded by white space or enclosed
                  in double quotation marks. */
} TOKEN;
     /* This is a list of recognized token types. */

struct Token
     /* This is a linked list of tokens giving type, text, and pointer to
        the next token. */
{
     TOKEN type;
          /* This is the token type.  It must have one of the values listed
             under the data type TOKEN. */
     char *text;
          /* This is a pointer to a character string containing the actual
             text of the token. */
     struct Token *next;
          /* This is a pointer to the next token structure in the list.
             When it is NULL there are no more tokens. */
};

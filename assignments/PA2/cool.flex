/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Donot remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
		YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

int comment_nesting = 0;

#define append_to_buffer(c) \
  do{ \
    if (string_buf_ptr >= string_buf + MAX_STR_CONST) { \
      cool_yylval.error_msg = "String constant too long"; \
      BEGIN(STRING_ERR); \
      return(ERROR); \
    } else { \
      *string_buf_ptr = c; \
      string_buf_ptr++; \
    } \
  }while(0)

/*
 *  Add Your own definitions here
 */

%}

DARROW          =>
ASSIGN          <-
LE              <=

 /* keyword */
CLASS           class
ELSE            else
FI              fi
IF              if
IN              in
INHERITS        inherits
ISVOID          isvoid
LET             let
LOOP            loop
POOL            pool
THEN            then
WHILE           while
CASE            case
ESAC            esac
NEW             new
OF              of
NOT             not

%x STRING
%x STRING_ERR
%x COMMENT
%%

 /* Ignore case */
\n             { curr_lineno++; }
[[:space:]]    { }

 /* Single-line comment */
--.*           { }

 /* Nested comments */
\(\*              { BEGIN(COMMENT); }
\*\)              { cool_yylval.error_msg = "Unmatched *)"; return(ERROR); }
<COMMENT>{
  \(\*     { comment_nesting++; }
  \\\(\*   { }
  \\\*\)   { }
  \*\)     {
             if (comment_nesting) {
               comment_nesting--;
             }else{
               BEGIN(INITIAL);
             }
           }
  \n       { curr_lineno++; }
  .        { }
  <<EOF>>  {
             BEGIN(INITIAL);
             cool_yylval.error_msg = "EOF in comment";
             return(ERROR);
           }
}

 /* Single-character operators. */
[\{\}\(\)\:\;\.]     { return (yytext[0]); }
[\+\-\*\/]           { return (yytext[0]); }
[\<\=\~\@\,]         { return (yytext[0]); }

 /* Multiple-character operators. */
{DARROW}		{ return (DARROW); }
{ASSIGN}    { return (ASSIGN); }
{LE}        { return (LE);}

 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */
(?i:{CLASS})      { return (CLASS); }
(?i:{ELSE})       { return (ELSE); }
(?i:{FI})         { return (FI); }
(?i:{IF})         { return (IF); }
(?i:{IN})         { return (IN); }
(?i:{INHERITS})   { return (INHERITS); }
(?i:{ISVOID})     { return (ISVOID); }
(?i:{LET})        { return (LET); }
(?i:{LOOP})       { return (LOOP); }
(?i:{POOL})       { return (POOL); }
(?i:{THEN})       { return (THEN); }
(?i:{WHILE})      { return (WHILE); }
(?i:{CASE})       { return (CASE); }
(?i:{ESAC})       { return (ESAC); }
(?i:{NEW})        { return (NEW); }
(?i:{OF})         { return (OF); }
(?i:{NOT})        { return (NOT); }

t(?i:rue)         {
                    cool_yylval.boolean = true;
                    return (BOOL_CONST); 
                  }
f(?i:alse)        {
                    cool_yylval.boolean = false;
                    return (BOOL_CONST); 
                  }

 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  */
\"                  {
                      string_buf_ptr = string_buf;
                      BEGIN(STRING);
                    }
<STRING>{
  \\n         {
                append_to_buffer('\n');
              }
  \\t         {
                append_to_buffer('\t');
              }
  \\b         {
                append_to_buffer('\b');
              }
  \\f         {
                append_to_buffer('\f');
              }

  \\\0        {
                cool_yylval.error_msg = "String contains escaped null character.";
                BEGIN(STRING_ERR);
                return(ERROR);
              }
  \\[^nbtf]   {
                if (yytext[1] == '\n') {
                  curr_lineno++;
                }
                append_to_buffer(yytext[1]);
              }

  "\""        {
                append_to_buffer('\0');
                cool_yylval.symbol = stringtable.add_string(string_buf);
                BEGIN(INITIAL);
                return (STR_CONST);
              }
  \n          {
                curr_lineno++;
                cool_yylval.error_msg = "Unterminated string constant";
                BEGIN(INITIAL);
                return (ERROR);
              }
  \0          {
                cool_yylval.error_msg = "String contains null character";
                BEGIN(STRING_ERR);
                return(ERROR);
              }
  .           {
                append_to_buffer(yytext[0]);
              }

  <<EOF>>     {
                BEGIN(INITIAL);
                cool_yylval.error_msg = "EOF in string constant";
                return (ERROR);
              }

}

<STRING_ERR>\\\n    { curr_lineno++; }
<STRING_ERR>\n      { BEGIN(INITIAL); }
<STRING_ERR>\"      { BEGIN(INITIAL); }
<STRING_ERR>.       { }

 /* Identifier */
[A-Z][_[:alnum:]]*  {
                      cool_yylval.symbol = idtable.add_string(yytext);
                      return (TYPEID); 
                    }
[a-z][_[:alnum:]]*  {
                      cool_yylval.symbol = idtable.add_string(yytext);
                      return (OBJECTID); 
                    }

 /* Integer */
[0-9]*              {
                      cool_yylval.symbol = inttable.add_string(yytext);
                      return (INT_CONST);
                    }

 /* Unmatched character */
.                   {
                      cool_yylval.error_msg = stringtable.add_string(yytext, 1)->get_string();
                      return (ERROR);
                    }

%%

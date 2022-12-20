%{
	#include "y.tab.h"
	extern YYSTYPE yylval;
%}

/***definitions***/
VALUEF 			[0-9]|[1-9][0-9]*
ID				[a-zA-Z][a-zA-Z0-9]*
STRING 			["](.*?)["]
COMMENT			";;".*
KW_NIL      "nil"
KW_APPEND   "append"
KW_CONCAT   "concat"
DEFF   		"deffun"
DEFV		"defvar"
KW_WHILE	"while"
KW_FOR      "for"
KW_IF       "if"
KW_EXIT     "exit"
KW_LOAD     "load"
KW_DISP     "disp"
KW_TRUE     "true"
KW_FALSE    "false"
OP_SET      "set"
OP_AND 		"and"
OP_OR 		"or"
OP_NOT		"not"
OP_EQ    	"eq"
OP_GT     	"gt"
OP_PLUS			"+"
OP_MINUS		"-"
OP_DIV			"/"
OP_MULT			"*"
OP 			"("
CP			")"
OP_COMMA		","


%%

(\r\n|\r|\n) {return 0;}
{OP_AND}		{return OP_AND;}
{OP_OR}			{return OP_OR;}
{OP_NOT}		{return OP_NOT;}
{OP_EQ}	    	{return OP_EQ;}
{OP_GT}	    	{return OP_GT;}
{KW_NIL}	    {return KW_NIL;}
{OP_SET}	    {return OP_SET;}
{DEFF}	    	{return DEFF;}
{DEFV}	    	{return DEFV;}
{KW_IF}		    {return KW_IF;}
{KW_EXIT}	    {return KW_EXIT;}
{KW_LOAD}	    {return KW_LOAD;}
{KW_WHILE}	    {return KW_WHILE;}
{KW_DISP}	    {return KW_DISP;}
{KW_TRUE}	    {return KW_TRUE;}
{KW_FALSE}	    {return KW_FALSE;}
{OP_PLUS}		{return OP_PLUS;}
{OP_MINUS}		{return OP_MINUS;}
{OP_DIV}		{return OP_DIV;}
{OP_MULT}		{return OP_MULT;}
{CP}			{return CP;}
{OP}			{return OP;}
{OP_COMMA}		{return OP_COMMA;}
{COMMENT}		{return COMMENT;}


[[:alpha:]][[:alnum:]]*   {strcpy(yylval.id, yytext); return ID;}
[[1-9][:digit:]]+         {yylval.idValue = atoi(yytext); return VALUEF;}
{VALUEF}					  {yylval.idValue = atoi(yytext); return VALUEF;}	
%%

int yywrap(){}
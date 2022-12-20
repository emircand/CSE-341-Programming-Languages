%{

#include <stdio.h>
#include <string.h>
#include "identifiers.h"

%}
%union
{
	int idValue;
    int *idtfvalues;
	char id[32];
}

%start INPUT

/* Defined tokens in lexer */
%token COMMENT OP_PLUS OP_MINUS OP_DIV OP_MULT OP_DBLMULT 
%token OP CP OP_COMMA KW_TRUE KW_FALSE
%token OP_AND OP_OR OP_NOT OP_EQ OP_GT
%token KW_NIL OP_SET DEFF DEFV KW_WHILE KW_IF KW_EXIT
%token KW_LOAD KW_DISP NEWLINE 


/* Defined tokens in parser */
%token <idValue> VALUEF
%token <id> ID
%type <idValue> INPUT
%type <idValue> EXP
%type <idValue> EXPB
/*%type <idtfvalues> VALUES*/
%type <idtfvalues> EXPLIST

%%
INPUT: 
    EXP {printf("SYNTAX OK. \nResult = %d\n", $1);}
    |
    EXPLIST {printf("SYNTAX OK. \nResult = ");}
    ;

EXP:
    OP OP_MULT EXP EXP CP  {$$=$3*$4;}
|   OP OP_DIV EXP EXP CP   {$$=$3/$4;} 
|   OP OP_PLUS EXP EXP CP  {$$=$3+$4;} 
|   OP OP_MINUS EXP EXP CP {$$=$3-$4;}  
|   VALUEF {$$ = $1;}
|   ID {Identifier* id = get($1); if (id == NULL) $$ = 0; else $$=id->value;}
|   OP OP_SET ID EXP CP {$$ = $4; put($3, $4);}             
|   OP KW_IF EXPB EXP CP {$$ = (1 == $3) ? $4: 0;}           
|   OP KW_IF EXPB EXP EXP CP {$$ = (1 == $3) ? $4: $5;}     
|   OP KW_WHILE EXPB EXP CP { $$ = (1 == $3) ? $4 : 0; }    
|   OP KW_DISP EXP CP { $$ = $3; printf("Disp: %d\n", $3);} 
;

EXPB:
    KW_TRUE  { $$ = 1; }  
|   KW_FALSE   { $$ = 0; } 
|   OP OP_AND EXPB EXPB CP { $$ = $3 && $4;} 
|   OP OP_OR EXPB EXPB CP  { $$ = $3 || $4; }   
|   OP OP_NOT EXPB CP  { $$ = !$3;}         
|   OP OP_GT EXP EXP CP { $$ = $3 > $4 ? 1 : 0; }  
|   OP OP_EQ EXPB EXPB CP { $$ = $3 == $4 ? 1 : 0; } 
|   OP OP_EQ EXP EXP CP { $$ = $3 == $4 ? 1 : 0; }
|   OP KW_DISP EXPB CP { $$ = $3; printf("Disp: %s\n", ($3 ? "True":"False"));}
|   OP DEFV ID EXP CP { $$ = $4; put($3, $4); }
;

EXPLIST: 
    OP EXP CP 
|   OP EXPLIST EXP CP
; 

%%

void yyerror(const char * s)
/* yacc error handler */
{  
 printf ("SYNTAX_ERROR Expression not recognized.\n");
}
  
int main(int argc, char **argv){
    create();
    printf("ENTER AN EXPRESSION: \n>> ");
    while(1){
            yyparse();
        }
    clear();
    return 0;
} 
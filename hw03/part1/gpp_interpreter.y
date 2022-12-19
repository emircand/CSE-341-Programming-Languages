%{

#include <stdio.h>
#include <string.h>
#include "gpp_interpreter.h"

int* add(int*, int);
int* concatenate(int*, int*);
int powerOf(int, int);
void print(int*);

%}

%union{
    int idValue;
    int *idtfvalues;
    char id[50];
};

%start INPUT

/* Defined tokens in lexer */
%token COMMENT OP_PLUS OP_MINUS OP_DIV OP_MULT OP_DBLMULT 
%token OP CP OP_COMMA KW_TRUE KW_FALSE
%token OP_AND OP_OR OP_NOT OP_EQ OP_GT
%token KW_NIL KW_LIST KW_APPEND KW_CONCAT 
%token OP_SET DEFF KW_WHILE KW_IF KW_EXIT
%token KW_LOAD KW_DISP NEWLINE STRING 


/* Defined tokens in parser */
%token <idValue> VALUEF
%token <id> ID

%type <idValue> INPUT
%type <idValue> EXP
%type <idValue> EXPB
%type <idtfvalues> VALUES
%type <idtfvalues> EXPLIST

%%
INPUT: 
    EXP {printf("SYNTAX OK. \nResult = %d\n", $1);}
    |
    EXPLIST {printf("SYNTAX OK. \nResult = "); print($1);}
    ;

EXP:
    OP OP_MULT EXP EXP CP  {$$=$3*$4;}  // (* EXP EXP) 
    |
    OP OP_DIV EXP EXP CP   {$$=$3/$4;}  // (/ EXP EXP) 
    |
    OP OP_PLUS EXP EXP CP  {$$=$3+$4;}  // (+ EXP EXP) 
    |
    OP OP_MINUS EXP EXP CP {$$=$3-$4;}  // (- EXP EXP) 
    |
    VALUEF {$$ = $1;}
    |
    ID {$$ = get($1);}
    |
    OP OP_SET ID EXP CP {$$ = $4; put($3, $4);}              // (set Id EXP)
    |
    OP KW_IF EXPB EXP CP {$$ = (1 == $3) ? $4: 0;}           // (if EXPB EXP) 
    |
    OP KW_IF EXPB EXP EXP CP {$$ = (1 == $3) ? $4: $5;}     // (if EXPB EXP EXP)
    |
    OP KW_WHILE EXPB EXP CP { $$ = (1 == $3) ? $4 : 0; }       // (for EXPB EXP)
    |
    OP KW_DISP EXP CP { $$ = $3; printf("Disp: %d\n", $3);} 
    ;

EXPB:
    KW_TRUE  { $$ = 1; }   // true
    |
    KW_FALSE   { $$ = 0; } // false
    | 
    OP OP_AND EXPB EXPB CP {$$ = $3 && $4;}   // (and EXPB EXPB) 
    |
    OP OP_OR EXPB EXPB CP  {$$ = $3 || $4;}    // (or EXPB EXPB) 
    |
    OP OP_NOT EXPB CP  {$$ = ! ($3);}         // (not EXPB)
    |
    OP OP_GT EXP EXP CP { $$ = $3 > $4; } // (greater than EXP EXP) 
    |
    OP OP_EQ EXPB EXPB CP {$$ = ($3 == $4);}  // (equal EXPB EXPB) 
    |
    OP OP_EQ EXP EXP CP {$$ = ($3 == $4);}  // (equal EXP EXP)
    |
    OP KW_DISP EXPB CP { $$ = $3; printf("Disp: %s\n", ($3 ? "True":"False"));}
    ;

/* 
    EXPLIST-> (concat EXPLIST EXPLIST) | (append EXP EXPLIST) | null 
*/
EXPLIST:
    OP KW_APPEND EXP EXPLIST CP {$$ = add($4, $3);}
    |
    OP KW_CONCAT EXPLIST EXPLIST CP {$$ = concatenate($3, $4);}
    |
    OP KW_LIST VALUES CP {$$ = $3;} 
    ;

VALUES: 
    VALUES VALUEF  {$$ = add($1, $2);}
    |
    VALUEF {$$ = NULL; $$ = add($$, $1);}
    ;

%%

/* ERROR messages */
int yyerror(char *s) {
    printf("SYNTAX ERROR. Expression not recognized\n");
    exit(-1);
}

/* Result */
void print(int *darray){

    printf("( ");
    for(int i=0; *(darray+i)!=-1; ++i)
        printf("%d ", *(darray+i));
    printf(")\n");
}

/* Function for adding new elements to the list */ 
int* add(int *darray, int num){    
    int *temp = darray;
    int size = 0, newSize = 0;     
    
    /* If the list is empty */
    if(temp == NULL){
        darray = (int *) malloc(sizeof(int)*2);
        *darray = num;
        *(darray+1) = -1;
        return darray;     
    } 
    
    /* If the list is not empty */
    while(*temp != -1)
        ++temp, ++size;

    /* If the list is full */
    temp = darray;
    darray = (int*)(malloc(sizeof(int) * (size+2)));
    for(int idx = 0 ; idx<size; ++idx)
        darray[idx] = temp[idx];
                  
    /* Adding the new element */
    darray[size] = num;                   
    darray[size+1] = -1;                  

    return darray;     
}

/*
 * This function is used to concatenate two lists.
 * It returns the new list.
 */
int* concatenate(int *darray1, int *darray2){
    int* tmp;
    int length_1=0, 
        length_2=0,
        idx = 0;
    /* Find the length of the first array */
    tmp = darray1;
    while(*tmp != -1)
        length_1++, tmp++;

    /* Find the length of the second array */
    tmp = darray2;
    while(*tmp != -1)
        length_2++, tmp++;

    /* Open new space for the concatenated array */
    tmp = (int *) malloc(sizeof(int) * (length_1 + length_2) + 3);

    /* Copy the first array into the new array */
    for(idx;idx<length_1;++idx)          
        tmp[idx] = darray1[idx];
    /* Copy the second array into the new array */
    for(int j = 0;j<length_2; ++j) 
        tmp[idx++] = darray2[j]; 

    /* sign for the end of the array */
    tmp[idx] = -1;

    return tmp;
}

int main(int argc, char **argv)
{
    printf("Enter the expression: ");
    ++argv, --argc;
    createSymbols();

    while(1){
        yyparse();
    }

    return 0;
}
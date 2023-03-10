%{

#include "global.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
int yyerror(char* msg);

        int recherche(char entite[]);

        void afficher(); 

        void inserer(char entite[]);

        void insererType(char entite[],char type[]);

        void insererEtat(char entite[],char etat[]);

        void insererTaille(char entite[],int Taille);

        void insererVal(char entite[],char val[]);

        int typeInt(char entite[]);

        int typeFloat(char entite[]);

        int typeString(char entite[]);

        int typeCar(char entite[]);

        int etatConst(char entite[]);

        int etatVar(char entite[]);

        char* getVal (char entite[]);

        char* getType (char entite[]);

        int retournerTaille (char entite[]);

        int declare(char entite[]);

        int constEd(char entite[]);
int yylex();
FILE *fp;
int nline;
int count;
char t[50] ;
%}

%union {

    int entier;
    char string[255];
    char c;
    float flt;
   
}



%start S
%token ENDINSTRUCT OPDOT WRITE READ
%token  AOPADD AOPDIV AOPMUL AOPMOD AOPSUB    
%token  LPFOR LPWHILE IF ELSE REPEAT 
%token  ASSIGN
%token  VERG
%token  LGUE RGUE
%token PROCEDURE PACKAGE ISMAIN NAMEPROG COMMENTAIRE
%token <string> TYPE
%token VAR CONST
%token <string> ISSTRING
%token ISTAB2D ISTAB1D
%token <entier> ISINT <flt> ISFLOAT <c> ISCHAR  NULLVALUE <string> BOOLTRUE <string> BOOLFALSE
%token <string> ISID
%token SAUT
%left LOPOR LOPAND 
%right LOPNOT   
%left ROPGT ROPLT ROPGE ROPLE ROPEQ ROPNE
%left PLUS  MOINS DIVISE FOIS PUISSANCE 
%right LPAR RPAR
%right LCROCH RCROCH




%%
S: ROPLT LOPNOT ISID ROPGT ENDINSTRUCT SLINE { 
if(declare($3)==0){ insererType($3,"program");
fprintf( fp, "~~~Le programme est syntaxiquement correct~~~\n");YYACCEPT;}
else {fprintf(fp, "erreur Semantique: double déclaration de la programme %s, avant la ligne %d, la colonne %d\n", $3, nline,count);}
}


;
SLINE: SCRIPT ENDINSTRUCT| SCRIPT ENDINSTRUCT SLINE
;
SCRIPT: DECLARATION_SCRIPT SCRIPT
|PROCEDURE_SCRIPT SCRIPT
| FUNCTION_SCRIPT SCRIPT
| SCRIPT_MAIN
;
DECLARATION_SCRIPT: PACKAGE ROPLT ISID ROPGE {

if(declare($3)==0){insererType($3,"package");}
else {fprintf(fp, "erreur Semantique: double déclaration de la package %s, avant la ligne %d, la colonne %d\n", $3, nline,count);}
}
| VAR TYPE ISID {
if(declare($3)==0){
insererType($3,"variable");
}
else {fprintf(fp, "erreur Semantique: double déclaration de la variable %s, avant la ligne %d, la colonne %d\n", $3, nline,count);}
}
| CONST TYPE ISID {
if(declare($3)==0){
insererType($3,"constant");
}
else  {fprintf(fp, "erreur Semantique: double déclaration de la constant %s, avant la ligne %d, la colonne %d\n", $2, nline,count);}
}
;
PROCEDURE_SCRIPT:PROCEDURE ISID LPAR  RPAR LCROCH PROGRAM RCROCH {
if(declare($2)==0){ insererType($2,"procedure");}
else {fprintf(fp, "erreur Semantique: double déclaration de la procédure %s, avant la ligne %d, la colonne %d\n", $2, nline,count);}}

| PROCEDURE ISID LPAR PARAMS RPAR LCROCH PROGRAM RCROCH {
if(declare($2)==0){ insererType($2,"procedure");}
else {fprintf(fp, "erreur Semantique: double déclaration de la procédure %s, avant la ligne %d, la colonne %d\n", $2, nline,count);}}
;
| FUNCTION_SCRIPT: TYPE ISID LPAR PARAMS RPAR LCROCH PROGRAM RCROCH { 
 if (declare($2)==0){

insererType($2,strcat("function",$2));
}
else {fprintf(fp, "erreur Semantique: double déclaration de la fonction %s, avant la ligne %d, la colonne %d\n", $2, nline,count);}
};

PARAMS : Param VERG PARAMS | Param;
Param: TYPE ISID {if (declare($2)==0)

		{   

			insererType($2,$1);

                  insererEtat($2,"var");

		}

		else  {fprintf(fp, "erreur Semantique: double déclaration de %s, avant la ligne %d, la colonne %d\n", $2, nline,count);}

        }

| TYPE ISID LPAR RPAR   {if (declare($2)==0)

				{   

					strcpy(t,"table");

					insererType($2,strcat(t,$1));

				}

				else {fprintf(fp, "erreur Semantique: double déclaration du tableau %s, avant la ligne %d, la colonne %d\n", $2, nline,count);}

    }


SCRIPT_MAIN: ISMAIN LCROCH PROGRAM RCROCH;
PROGRAM: STATEMENT PROGRAM | ;

STATEMENT: IF_STATEMENT
| ASSIGN_STATEMENT
| FOR_STATEMENT
| WHILE_STATEMENT
| WRITE_STATEMENT
| READ_STATEMENT
;
IF_STATEMENT: IF  CONDITION  {
fprintf(fp, "***Les routines sémantiques pour l'instruction if-else***\n");
    fprintf(fp, "IF - R1\n");
    fprintf(fp, "QUAD (BZ, , <CONDITION>, ) \n");
    fprintf(fp, "Sauv-BZ := Qc \n");
    fprintf(fp, "Qc := Qc+1 \n");
} LCROCH PROGRAM RCROCH {
fprintf(fp, "IF - R2\n");
    fprintf(fp, "QUAD (Qc) := (BR, , , ) \n");
    fprintf(fp, "Sauv-BR := Qc \n");
    fprintf(fp, "Qc := Qc + 1 \n");
    fprintf(fp, "QUAD (Sauv-BZ, 2) := Qc \n");

}
REST {
 
    fprintf(fp, "IF - R5\n");
    fprintf(fp, "QUAD (Sauv-BR, 2) := Qc \n"); //fin
    fprintf(fp, "******\n\n");

}
;
REST: ELSE OPDOT LCROCH PROGRAM RCROCH
;
CONDITION:LPAR COMPARAISON RPAR
| LPAR ROPNE CONDITION RPAR
| LPAR CONDITION LOPAND CONDITION RPAR
| LPAR CONDITION LOPOR CONDITION RPAR
; 

COMPARATEUR: ROPLT|ROPGT|ROPLE|ROPGE|ROPEQ|ROPNE;
COMPARAISON: EXPRESSION COMPARATEUR EXPRESSION;
EXPRESSION: EXPRESSION OP EXPRESSION | LPAR EXPRESSION RPAR | LOPNOT EXPRESSION | ATR;
OP: AOPSUB | AOPMUL | AOPDIV | AOPMOD | LOPAND | LOPOR
ATR: ISID 
{ 
if(declare($1)==0)

                        {fprintf(fp, "erreur Semantique: %s non declaré, avant la ligne %d, a la colonne %d\n", $1, nline,count);} 
 else{

                            if (etatConst($1)==1)

                            {

                                {fprintf(fp, "erreur Semantique:  %s est une constante: avant la ligne %d, a la colonne %d\n",$1, nline,count);}                             

                            }

                            else if( typeInt($1)!=1 && typeFloat($1)!=1)

                            {fprintf(fp, "erreur Semantique: type de %s est incompatible  : avant la ligne %d, a la colonne %d\n",$1, nline,count);}      
}
}
;
ASSIGN_STATEMENT: ISID ASSIGN EXPRESSION {
if(declare($1)==0){fprintf(fp, "erreur Semantique: %s non declaré, avant la ligne %d, a la colonne %d\n", $1, nline,count);}
}
;
FOR_STATEMENT: LPFOR LPAR CONDITION  RPAR OPDOT LCROCH PROGRAM RCROCH
;
WHILE_STATEMENT: LPWHILE LPAR EXPRESSION RPAR OPDOT LCROCH PROGRAM RCROCH
;
WRITE_STATEMENT: WRITE LPAR EXPRESSION RPAR
;
READ_STATEMENT: READ LPAR ISID RPAR
;


%%

int main()

{
    fp = fopen("Output.txt", "w");
    yyparse();
    printf("a");
   

} 

int yyerror(char *msg)

{

    fprintf(fp, "Erreur syntaxique rencontree a la ligne %d, la colonne %d\n",nline,count);

}




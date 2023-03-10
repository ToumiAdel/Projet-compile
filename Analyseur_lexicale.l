
%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h> 
#include "parser.tab.h"
#include "Table_Symboles.h"
extern YYSTYPE yylval;

    int col=0 ;
    int linenumber=1 ;
%}
letter [a-zA-Z]

sautLigne "\015"

commentaire  (([a-zA-Z0-9,?!.’ “()<>{}#%^*+=_\|~])|([[])|([]])|("\015")|("\n"))

char [']{letter}[']

string [']((({letter}{letter}+))|(""))[']

int (([1-9][0-9]*)|0)

float {int}+"."{int}

ID [a-zA-Z_][a-zA-Z0-9_]*

bool ("True" | "False")


type (("int")|("bool")|("float")|("char")|("enum")|("structure"))

tableau2D {ID}[[]{int}[]][[]{int}[]]

tableau1D {ID}[[]{int}[]]


package "UsePackage"

procedure "void"

main "main"

%%


"/*"{commentaire}*"*/" {col += yyleng; return COMMENTAIRE;}



{main}  { col += yyleng; return ISMAIN;} 

{procedure} { col += yyleng; return PROCEDURE;}

{package} { return PACKAGE;}

"+" { return AOPADD;}
"/" {return AOPDIV;}
"*" {return AOPMUL;}
"%" {return AOPMOD;}
"-" {return AOPSUB;}

"("  {return LPAR;}
")"  {return RPAR;}
">"  { return ROPGT;}
"<"  { return ROPLT;}
">=" { return ROPGE;}
"<=" { return ROPLE;}
"==" { return ROPEQ;}
"<>" {return ROPNE;}
"&&"  { return LOPAND;}
"||" {return LOPOR;}
"!" {return LOPNOT;}

"LpFor"	{return LPFOR;} 


"LPWhile" {return LPWHILE;}
"if" {return IF;}
"else" {return ELSE;}
"RepeatWhile" {return REPEAT;} 
"Readit" {return READ;}
"ShowOn" {return WRITE;} 
"“" {return LGUE;}
"”" {return RGUE;}
":" {return OPDOT;}
"{"  {return LCROCH;}
"}"  {return RCROCH ;}
"="  { return ASSIGN;}
"," {return VERG;}



{type} { return TYPE;}


"var" {return VAR;}

"const" {CONST;}

{string} {return ISSTRING;}


{tableau2D} {return ISTAB2D;} 

{tableau1D} {return ISTAB1D;}

{int} {return ISINT;}

{char} {return ISCHAR;}

{float} { return ISFLOAT;}

"NULL" { return NULLVALUE ;}

"True" {return BOOLTRUE;}

"False" {return BOOLFALSE;}



{ID} {inserer(yytext);insererType(yytext,"Identifiant");return ISID;}




"\015" {return SAUT;}


";" {return ENDINSTRUCT;}

" " {}

. {printf("erreur lexicale ligne : %d | colonne : %d \n",linenumber,col);}




%%





int yywrap(){
   return 1 ;
}  


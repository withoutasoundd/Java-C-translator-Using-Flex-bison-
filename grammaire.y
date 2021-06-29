%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
extern void yyerror(const char *s);
extern int yylex();
extern int yyparse();
extern int yylineno;
char *cat(char *path, const char *directory);
char *nonspace(char *path, const char *directory);
%}

%token <string> ID ENTIER STRING PRNT MAIN ARGS
%token <string> CHAR BOOLEEN INT DOUBLE LONG FLOAT SHORT BYTE
%token <string> DO WHILE FOR IF ELSE SWITCH CASE BREAK NEW
%token <string> VOID CLASS PUBLIC PRIVATE PROTECTED RETURN STATIC FINAL CONTINUE DEFAULT
%token <string> PACKAGE INTER EXTENDS IMPLEMENT IMPORT ABSTRACT 
%token <string> FINALLY T_QUOTE
%token <string> THIS THROW THROWS TRY SUPER CATCH
%token <string> NON ETT OUU OU ET VIRGULE DEUXP ACCOLADEG ACCOLADED  POINT PARG PARD CROCHG CROCHD
%token <string> REC PLREC MREC MUREC DIREC ETREC OUREC MODREC PV PP MM PLUS SUB MULT DIV MOD INF SUPP INFEG SUPPEG EGAL DIFFERENT NOT
%left OU 
%left ET
%left PLUS SUB 
%left MULT DIV
%right EGAL

%union{
  char *string;
  }

%type <string> File PackageDecl ImporteDeclsOpt ImporteDecls ImporteDecl TypeDeclsOpt TypeDecls TypeDecl SingleImportDeclaration 
%type <string> Classe Supere Interfaces InterfaceList Interface InterfaceBody InterfaceMemberDeclarationsOpt ConstantDeclaration AbstractMethodDeclaration 
%type <string> ClassBody ClassBodyDeclarations ClassBodyDeclaration BlockStatementsOpt BlockStatements BlockStatement Statement StatementWithoutTrailingSubstatement
%type <string> EmptyStatement LabeledStatement IfThenElseStatement SwitchStatement SwitchBlock SwitchBlockStatementGroupsOpt SwitchBlockStatementGroups SwitchBlockStatementGroup
%type <string> SwitchLabel WhileStatement DoStatement ForStatement BreakStatement ContinueStatement ReturnStatement IDOpt ForInitOpt ExpressionOpt ForUpdateOpt ForInit ForUpdate
%type <string> StatementExpression StatementExpressionList LocalVariableDeclarationStatement LocalVariableDeclaration ClassMemberDeclaration FieldDeclaration
%type <string> VariableDeclarators VariableDeclarator VariableDeclaratorId VariableInitializer ArrayInitializer VariableInitializersOpt VariableInitializers MethodDeclaration
%type <string> MethodHeader MethodDeclarator MethodBody StaticInitializer ConstructorDeclaration ConstructorDeclarator ConstructorBody FormalParameterList FormalParameter
%type <string> Expression Assignment ConditionalExpression ConditionalOrExpression ConditionalAndExpression
%type <string> InclusiveOrExpression AndExpression EqualityExpression RelationalExpression AdditiveExpression MultiplicativeExpression UnaryExpression PreIncrementExpression
%type <string> PreDecrementExpression UnaryExpressionNotPlusMinus PostFixExpression PostIncrementExpression PostDecrementExpression Primary ArrayCreationExpression
%type <string> DimExprs DimExpr PrimaryNoNewArray ClassInstanceCreationExpression MethodInvocation ArgumentListOpt ArgumentList LeftHandSide FieldAccess
%type <string> ArrayAccess AssignmentOperator Type ACCESMOD NAME SimpleName QualifiedName Printer MainDeclaration ElseOpt ExpressionStatement chaine Throwse
%type <string> ThrowStatement TryStatement Catches CatchClause Finallyy Nombre MethodHeaderinter mainsat mainsats nameopt maininstance Opr
Printere

%start program
%%
program : File {printf(" #include <iostream>\n using namespace std;\n %s",$1);}
	;

File : PackageDecl ImporteDeclsOpt TypeDeclsOpt {$$=cat(cat($1,$2),$3);}
	;
		
PackageDecl : PACKAGE NAME PV {$$=nonspace(cat("using namespace",$2),";\n");}    
	| { $$ = ""; } 
	;
	
ImporteDeclsOpt: ImporteDecls {$$=$1;}
	| { $$ = ""; } 
	;
		
ImporteDecls: ImporteDecl {$$=$1;}
	| ImporteDecls ImporteDecl {$$=cat($1,$2);}
	;
	
ImporteDecl: SingleImportDeclaration   {$$=$1;}
        ;

TypeDeclsOpt: TypeDecls {$$=$1;}
	| { $$ = ""; } 
	; 
	 
TypeDecls: TypeDecl {$$=$1;}
	| TypeDecls TypeDecl {$$=cat($1,$2);}
	;
	
TypeDecl: Classe  {$$=$1;}
	| Interface  {$$=$1;}
	;
	
SingleImportDeclaration: IMPORT NAME PV {$$=nonspace(nonspace("#include<",$2),">\n");} 
        ;


Classe: ACCESMOD CLASS ID Supere Interfaces ClassBody {$$=cat(cat(cat(cat("class",$3),$4),$5),$6);}
        ;

Supere: EXTENDS NAME   {$$=cat(":",$2);}
	| { $$ =""; } 
	;
	
Throwse: THROWS NAME  {$$="";}
        | {$$="";}
		;
			
Interfaces: IMPLEMENT InterfaceList  {$$=cat(": public",$2);}
	| { $$ =""; } 
        ;

InterfaceList: NAME {$$=$1;}
        | InterfaceList VIRGULE NAME     {$$=nonspace(nonspace($1,","),$3);}
        ;
        
Interface:INTER NAME ACCOLADEG InterfaceBody ACCOLADED   {$$=cat(cat(cat(cat("class ",$2),"{\n"),$4),"}\n");}
         ;   

InterfaceBody: InterfaceMemberDeclarationsOpt  {$$=$1;}
        | InterfaceBody InterfaceMemberDeclarationsOpt {$$=cat($1,$2);}
	;
	
InterfaceMemberDeclarationsOpt: ConstantDeclaration   {$$=$1;} 
        | AbstractMethodDeclaration    {$$=$1;}
        ;
        
ConstantDeclaration: FieldDeclaration    {$$=cat("virtual",$1);}
	;
	        
AbstractMethodDeclaration: MethodHeaderinter PV   {$$=nonspace($1,";\n");}   
	;

MethodHeaderinter: ACCESMOD Type MethodDeclarator  {$$=cat(cat("virtual",$2),$3);}
        |  ACCESMOD VOID MethodDeclarator    {$$=cat(cat("virtual","void"),$3);}
       ;

ClassBody: ACCOLADEG ClassBodyDeclarations ACCOLADED   {$$=cat(cat("{\n",$2),"}\n");}
         ;
MainDeclaration: ACCESMOD STATIC VOID MAIN PARG STRING CROCHG CROCHD ARGS PARD ACCOLADEG ClassBodyDeclarations ACCOLADED   {$$=cat(cat("int main() {\n",$12),"return 0; }\n");}
          ;

ClassBodyDeclarations: ClassBodyDeclaration {$$=$1;}
        | ClassBodyDeclarations ClassBodyDeclaration {$$=cat($1,$2);}
        ;

ClassBodyDeclaration: ClassMemberDeclaration   {$$=$1;}
        | StaticInitializer    {$$=$1;}
        | ConstructorDeclaration   {$$=$1;}
        | MainDeclaration {$$=$1;}
        | Printer        {$$=$1;}
        | mainsats        {$$=$1;}
        ;
mainsats: mainsat {$$=$1;}
        | mainsats mainsat {$$=cat($1,$2);}
        ;
        
mainsat: IfThenElseStatement   {$$=$1;}
        | WhileStatement     {$$=$1;}
        | ForStatement    {$$=$1;}
        | DoStatement     {$$=$1;}
        | SwitchStatement   {$$=$1;}
        | ThrowStatement   {$$=$1;}
        | TryStatement    {$$=$1;}
        | maininstance     {$$=$1;}
        ;

maininstance: nameopt NAME REC Primary PV  {$$=nonspace(cat(cat(cat($1,$2),"="),$4),";\n");}
        | NAME REC Primary PV  {$$=nonspace(cat(cat($1,"="),$3),";\n");}
        | Primary PV  {$$=nonspace($1,";\n");} 
        ; 
               
nameopt: ID    {$$=$1;}	
	;
         
BlockStatementsOpt: BlockStatements  {$$=$1;}
	| { $$ = ""; } 
	;

BlockStatements: BlockStatement  {$$=$1;}
        | BlockStatements BlockStatement {$$=cat($1,$2);}
        ;

BlockStatement: LocalVariableDeclarationStatement  {$$=$1;}
        | Statement    {$$=$1;}
        ;

Statement: StatementWithoutTrailingSubstatement  {$$=$1;}
        | LabeledStatement   {$$=$1;}
        | IfThenElseStatement   {$$=$1;}
        | WhileStatement   {$$=$1;}
        | ForStatement    {$$=$1;}
        | Printer        {$$=$1;}
        ;

StatementWithoutTrailingSubstatement: ACCOLADEG BlockStatementsOpt ACCOLADED   {$$=cat(cat("{",$2),"}\n");}
        | EmptyStatement             {$$=$1;}
        | SwitchStatement            {$$=$1;}
        | DoStatement                 {$$=$1;}
        | BreakStatement               {$$=$1;}
        | ContinueStatement           {$$=$1;}
        | ReturnStatement            {$$=$1;}
        | ExpressionStatement        {$$=$1;}
        | ThrowStatement             {$$=$1;}
        | TryStatement               {$$=$1;}
        ;
        
ExpressionStatement: StatementExpression PV {$$=cat($1,";\n");}
        ;        
        
Printer: PRNT PARG T_QUOTE chaine T_QUOTE PARD PV       {$$=nonspace(nonspace("cout << \"",$4),"\";\n");}
        |PRNT PARG Printere PARD PV            {$$=nonspace(cat("cout <<",$3),";\n");}
        |PRNT PARG T_QUOTE chaine T_QUOTE PLUS Printere PARD PV    {$$=cat(nonspace(cat(nonspace("cout << \"",$4),"\"<<"),$7),";\n");}
        
        ;
       
Printere: ID {$$=$1;}
	| Opr ID {$$=nonspace($1,$2);}
	| Printere Opr ID {$$=nonspace(nonspace($1,$2),$3);}
	;
        
        
chaine: NAME   {$$=$1;}
        | chaine NAME  {$$=cat($1,$2);}
        ;

EmptyStatement: PV    {$$=";\n";}
        ;

LabeledStatement: ID DEUXP Statement     {$$=cat(cat(nonspace($1,":"),$3),"\n");}  
        ;

ElseOpt: ELSE ACCOLADEG BlockStatementsOpt ACCOLADED {$$=cat(cat("else {",$3),"}\n");}
         | { $$ = ""; }
         ;

IfThenElseStatement: IF PARG Expression PARD ACCOLADEG BlockStatementsOpt ACCOLADED ElseOpt      {$$=cat(cat(cat(nonspace(nonspace("if (",$3),") {\n"),$6),"}\n"),$8);}
        ;


SwitchStatement: SWITCH PARG Expression PARD SwitchBlock               {$$=cat(cat(cat("switch (",$3),")"),$5);}
        ;

SwitchBlock: ACCOLADEG SwitchBlockStatementGroupsOpt ACCOLADED      {$$=cat(cat("{\n",$2),"}\n");}
        ;

SwitchBlockStatementGroupsOpt: SwitchBlockStatementGroups    {$$=$1;}
	| { $$ = ""; } 
	;

SwitchBlockStatementGroups: SwitchBlockStatementGroup     {$$=$1;}
        | SwitchBlockStatementGroups SwitchBlockStatementGroup      {$$=cat($1,$2);}
        ;

SwitchBlockStatementGroup: SwitchLabel BREAK PV       {$$=cat($1,"break;\n");} 
        ;

SwitchLabel: CASE Expression DEUXP BlockStatementsOpt        {$$=cat(nonspace(cat("case",$2),":"),$4);}  
        | DEFAULT DEUXP BlockStatementsOpt         {$$=cat("default: ",$3);}
        ;                  

WhileStatement: WHILE PARG Expression PARD ACCOLADEG BlockStatementsOpt ACCOLADED       {$$=cat(cat(nonspace(nonspace("while (",$3),") {\n"),$6),"}\n");}
        ;


DoStatement: DO ACCOLADEG BlockStatementsOpt ACCOLADED WHILE PARG Expression PARD PV        {$$=nonspace(nonspace(cat(cat("do {\n",$3),"}\n while ("),$7),");\n");}
        ;

ForStatement: FOR PARG ForInitOpt PV ExpressionOpt PV ForUpdateOpt PARD ACCOLADEG BlockStatementsOpt ACCOLADED     {$$=cat(cat(nonspace(cat(nonspace(cat(nonspace(nonspace("for (",$3),";"),$5),";"),$7),") {\n"),$10),"}\n");}
            ; 
            
ThrowStatement:  THROW Expression PV   {$$=cat(cat("throw",$2),";\n");}
	; 
		
TryStatement: TRY ACCOLADEG BlockStatementsOpt ACCOLADED Catches    {$$=cat(cat(cat("try {\n",$3),"}\n"),$5);}
	| TRY ACCOLADEG BlockStatementsOpt ACCOLADED Catches Finallyy  {$$=cat(cat(cat(cat("try {\n",$3),"}\n"),$5),$6);}
	;          

Catches: CatchClause   {$$=$1;}
	| Catches CatchClause   {$$=cat($1,$2);}
	;
	
CatchClause: CATCH PARG chaine PARD ACCOLADEG BlockStatementsOpt ACCOLADED   {$$=cat(cat(cat(cat("catch (",$3),") {\n"),$6),"}\n");}
	;

Finallyy: FINALLY ACCOLADEG BlockStatementsOpt ACCOLADED   {$$=cat(cat("finally {\n",$3),"}\n");}
	;		
		
BreakStatement: BREAK IDOpt PV     {$$=nonspace(cat("break",$2),";\n");}
        ;

ContinueStatement: CONTINUE IDOpt PV     {$$=nonspace(cat("continue",$2),";\n");}
        ;

ReturnStatement: RETURN ExpressionOpt PV   {$$=nonspace(cat("return",$2),";\n");}
        ;
      
IDOpt: ID   {$$=$1;}
	| { $$ = ""; } 
	;

ForInitOpt: ForInit   {$$=$1;}
	| { $$ = ""; } 
	;

ExpressionOpt: Expression   {$$=$1;}
	| { $$ = ""; } 
	;

ForUpdateOpt: ForUpdate   {$$=$1;}
	| { $$ = ""; } 
	;

ForInit: StatementExpressionList     {$$=$1;}
        | LocalVariableDeclaration      {$$=$1;}
        ;

ForUpdate: StatementExpressionList    {$$=$1;}
        ;

StatementExpression: Assignment     {$$=$1;}
        | PreIncrementExpression    {$$=$1;}
        | PreDecrementExpression    {$$=$1;}
        | PostIncrementExpression    {$$=$1;}
        | PostDecrementExpression    {$$=$1;}
        | MethodInvocation          {$$=$1;}
        | ClassInstanceCreationExpression    {$$=$1;}
        | ArrayCreationExpression         {$$=$1;}
        ;
StatementExpressionList: StatementExpression    {$$=$1;}
        | StatementExpressionList VIRGULE StatementExpression      {$$=cat(cat($1,","),$3);}
        ;
                           
LocalVariableDeclarationStatement: LocalVariableDeclaration PV      {$$=nonspace($1,";\n");}
        ;
 
LocalVariableDeclaration: Type VariableDeclarators        {$$=cat($1,$2);}
        ;
                              
ClassMemberDeclaration: FieldDeclaration  {$$=$1;}
        | MethodDeclaration   {$$=$1;}
        | LocalVariableDeclarationStatement {$$=$1;}
        ;  

FieldDeclaration: ACCESMOD Type VariableDeclarators PV    {$$=nonspace(cat(cat($1,$2),$3),";\n");} 
	;  
	    
VariableDeclarators: VariableDeclarator           {$$=$1;}
        | VariableDeclarators VIRGULE VariableDeclarator        {$$=nonspace(nonspace($1,","),$3);  }
        ;

VariableDeclarator: VariableDeclaratorId     {$$=$1;}
        | VariableDeclaratorId REC VariableInitializer   {$$=cat(nonspace($1,"="),$3);  }
        ;

VariableDeclaratorId: ID    {$$=$1;}
        | VariableDeclaratorId CROCHG CROCHD  {$$=cat($1,"[]");}
        ;

VariableInitializer: Expression   {$$=$1;}
        | ArrayInitializer    {$$=$1;}
        ;
        
ArrayInitializer: ACCOLADEG VariableInitializersOpt ACCOLADED        {$$=cat(cat("{",$2),"}\n");}
        ;

VariableInitializersOpt: VariableInitializers     {$$=$1;}
	| { $$ = ""; } 
	;

VariableInitializers: VariableInitializer     {$$=$1;}
        | VariableInitializers VIRGULE VariableInitializer      {$$=nonspace(nonspace($1,","),$3);  }
        ;
                              
MethodDeclaration: MethodHeader MethodBody   {$$=cat($1,$2);}
        ;

MethodHeader: ACCESMOD Type MethodDeclarator Throwse      {$$=cat(cat(cat($1,$2),$3),$4);}
        |  ACCESMOD VOID MethodDeclarator Throwse      {$$=cat(cat(cat($1,"void"),$3),$4);}
        ;
        
MethodDeclarator: ID PARG FormalParameterList PARD      {$$=nonspace(nonspace(nonspace($1,"("),$3),")");}
        | MethodDeclarator CROCHG CROCHD        {$$=nonspace($1,"[]");  }
        ;
        
MethodBody: ACCOLADEG BlockStatementsOpt ACCOLADED      {$$=cat(cat("{\n",$2),"\n}\n");}
	;

StaticInitializer: STATIC ACCOLADEG BlockStatementsOpt ACCOLADED      {$$=cat(cat("static {\n",$3),"\n}\n");}
        ;

ConstructorDeclaration: PUBLIC ConstructorDeclarator ConstructorBody     {$$=cat($2,$3);}
        ;

ConstructorDeclarator: ID PARG FormalParameterList PARD       {$$=nonspace(nonspace(nonspace($1,"("),$3),")");}    
        ;

ConstructorBody: ACCOLADEG BlockStatementsOpt ACCOLADED       {$$=cat(cat("{\n",$2),"\n}\n");}
        ;

FormalParameterList: FormalParameter      {$$=$1;}
        | FormalParameterList VIRGULE FormalParameter    {$$=nonspace(nonspace($1,","),$3);}
        |{ $$ = ""; } 
        ;	

FormalParameter: Type ID   {$$=cat($1,$2);}
        ;
                	

Expression: ConditionalExpression            {$$=$1;}
        | Assignment           {$$=$1;}
        ;
        
Assignment:      LeftHandSide AssignmentOperator Expression    {$$=cat(cat($1,$2),$3);}
        ;
                
ConditionalExpression: ConditionalOrExpression          {$$=$1;}
	;

ConditionalOrExpression: ConditionalAndExpression            {$$=$1;}
        | ConditionalOrExpression OUU ConditionalAndExpression        {$$=cat(cat($1,"||"),$3);}
        ;

ConditionalAndExpression: InclusiveOrExpression                  {$$=$1;}
        | ConditionalAndExpression ETT InclusiveOrExpression     {$$=cat(cat($1,"&&"),$3);}
        ;

InclusiveOrExpression: AndExpression                       {$$=$1;}
        | InclusiveOrExpression OU AndExpression           {$$=cat(cat($1,"|"),$3);}
        ;
        
AndExpression: EqualityExpression                               {$$=$1;}
        | AndExpression ET EqualityExpression                   {$$=cat(cat($1,"&"),$3);}
        ;

EqualityExpression: RelationalExpression                         {$$=$1;}
        | EqualityExpression EGAL RelationalExpression           {$$=cat(cat($1,"=="),$3);}
        | EqualityExpression DIFFERENT RelationalExpression      {$$=cat(cat($1,"!="),$3);}
        ;

RelationalExpression: AdditiveExpression                         {$$=$1;}
        | RelationalExpression INF AdditiveExpression           {$$=cat(cat($1,"<"),$3);}
        | RelationalExpression SUPP AdditiveExpression           {$$=cat(cat($1,">"),$3);}
        | RelationalExpression INFEG AdditiveExpression           {$$=cat(cat($1,"<="),$3);}
        | RelationalExpression SUPPEG AdditiveExpression           {$$=cat(cat($1,">="),$3);}
        ;

AdditiveExpression: MultiplicativeExpression                        {$$=$1;}
        | AdditiveExpression PLUS MultiplicativeExpression          {$$=cat(cat($1,"+"),$3);}
        | AdditiveExpression SUB MultiplicativeExpression           {$$=cat(cat($1,"-"),$3);}
        ;

MultiplicativeExpression: UnaryExpression                               {$$=$1;}
        | MultiplicativeExpression MULT UnaryExpression                {$$=cat(cat($1,"*"),$3);}
        | MultiplicativeExpression DIV UnaryExpression                 {$$=cat(cat($1,"/"),$3);}
        | MultiplicativeExpression MOD UnaryExpression                 {$$=cat(cat($1,"%"),$3);}
        ;

UnaryExpression:  PreIncrementExpression          {$$=$1;}
        | PreDecrementExpression                  {$$=$1;}
        | SUB UnaryExpression                     {$$=nonspace("-",$2);}
        | PLUS UnaryExpression                    {$$=nonspace("+",$2);}
        | UnaryExpressionNotPlusMinus             {$$=$1;}
        ;

PreIncrementExpression: PP UnaryExpression   {$$=nonspace("++",$2);}
        ;

PreDecrementExpression: MM UnaryExpression    {$$=nonspace("--",$2);}
        ;

UnaryExpressionNotPlusMinus: PostFixExpression  {$$=$1;}
        | NON UnaryExpression           {$$=nonspace("~",$2);}
        | NOT UnaryExpression           {$$=nonspace("!",$2);}
        ;

PostFixExpression: Primary    {$$=$1;}
        | NAME                {$$=$1;}
        | PostIncrementExpression    {$$=$1;}
        | PostDecrementExpression     {$$=$1;}
        | Nombre  {$$=$1;}
        ;
        
Nombre: ENTIER {$$=$1;}
        | ENTIER POINT ENTIER  {$$=nonspace(nonspace($1,"."),$3);}
        ;  
            
PostIncrementExpression: PostFixExpression PP    {$$=nonspace($1,"++");}
        ;
        
PostDecrementExpression: PostFixExpression MM    {$$=nonspace($1,"--");}
        ;

Primary:      PrimaryNoNewArray    {$$=$1;}
        | ArrayCreationExpression    {$$=$1;}
        ;
        
ArrayCreationExpression: NEW Type DimExprs      {$$=cat($2,$3);}
        | NEW NAME DimExprs              {$$=cat($2,$3);}
        | Type DimExprs NAME                       {$$=cat(cat($1,$3),$2);} 
        | ArrayCreationExpression REC ArrayCreationExpression    {$$=cat(cat($1,"="),$3);}              
        ;

DimExprs: DimExpr  {$$=$1;}
        | DimExprs DimExpr    {$$=cat($1,$2);}
        ;

DimExpr: CROCHG Expression CROCHD      {$$=nonspace(nonspace("[",$2),"]");}
        | CROCHG CROCHD    {$$="[]";} 
        ;


PrimaryNoNewArray: THIS  {$$="this";}
        | PARG Expression PARD   {$$=nonspace(nonspace("(",$2),")");}
        | ClassInstanceCreationExpression   {$$=$1;}
        | FieldAccess    {$$=$1;}
        | MethodInvocation   {$$=$1;}
        | ArrayAccess   {$$=$1;}
        ;

ClassInstanceCreationExpression: NEW ID PARG ArgumentListOpt PARD     {$$=nonspace(nonspace(cat(cat("new",$2),"("),$4),")");}
        ;

MethodInvocation: NAME PARG ArgumentListOpt PARD        {$$=nonspace(nonspace(cat($1,"("),$3),")");}
        | Primary POINT ID PARG ArgumentListOpt PARD      {$$=cat(cat(cat(nonspace(nonspace($1,"."),$3),"("),$5),")");}
        | SUPER POINT ID PARG ArgumentListOpt PARD          {$$=nonspace(nonspace(nonspace(nonspace("super::",$3),"("),$5),")");}
        | NAME ACCOLADEG ArgumentListOpt ACCOLADED            {$$=cat(cat(cat($1,"{"),$3),"}\n");}
        | Primary POINT ID ACCOLADEG ArgumentListOpt ACCOLADED        {$$=cat(cat(cat(nonspace(nonspace($1,"."),$3),"{"),$5),"}");}
        | SUPER POINT ID ACCOLADEG ArgumentListOpt ACCOLADED          {$$=cat(cat(cat(nonspace("super::",$3),"{"),$5),"}");}
        | SUPER PARG ArgumentListOpt PARD       {$$=nonspace(nonspace("super (",$3),")");}
        ;

ArgumentListOpt: ArgumentList   {$$=$1;}
	| { $$ = ""; }  
	;

ArgumentList: Expression    {$$=$1;}
        | ArgumentList VIRGULE Expression  {$$=nonspace(nonspace($1,","),$3);}
        ;
	       
LeftHandSide: NAME {$$=$1;}
        | FieldAccess  {$$=$1;}
        | ArrayAccess   {$$=$1;}
        ;

FieldAccess: Primary POINT ID    {$$=nonspace(nonspace($1,"->"),$3);}
        | SUPER POINT ID        {$$=nonspace("super::",$3);}
        ;
                
ArrayAccess: NAME CROCHG Expression CROCHD        {$$=nonspace(nonspace(cat($1,"["),$3),"]");}
        | PrimaryNoNewArray CROCHG Expression CROCHD        {$$=nonspace(nonspace(cat($1,"["),$3),"]");}
        ; 

AssignmentOperator: REC  {$$="=";} 
        | MUREC     {$$="*=";} 
        | DIREC     {$$="/=";}
        | MODREC     {$$="%=";}
        | PLREC      {$$="+=";}
        | MREC     {$$="-=";} 
        | ETREC     {$$="&=";}
        | OUREC     {$$="|=";}
        ;                       	                

Type: BYTE       {$$=$1;} 
	|SHORT   {$$="short";} 
	|INT      {$$="int";} 
	|DOUBLE   {$$="double";} 
	|LONG       {$$="long";} 
	|FLOAT    {$$="float";} 
	|CHAR     {$$="char";} 
	|BOOLEEN   {$$="bool";} 
	|STRING {$$="String";}
	;
	
ACCESMOD:PUBLIC  {$$="public";} 
        | PROTECTED  {$$="protected";}
        | PRIVATE  {$$="private";} 
        | STATIC  {$$="static";} 
        | ABSTRACT  {$$="Abstract";} 
        | FINAL  {$$="const";} 
        | { $$ = ""; }  
        ;
        
Opr: PLUS { $$ = "+"; }
	|SUB { $$ = "-"; }
	|MULT { $$ = "*"; }
	|DIV { $$ = "/"; }
	|MOD { $$ = "%"; }
;
NAME:   SimpleName {$$=$1;}
        | QualifiedName {$$=$1;}
        ;
        
SimpleName:ID  {$$=$1;} 
        ;
        
QualifiedName:NAME POINT ID {$$=nonspace(nonspace($1,"."),$3);}
        ;
      
%% 

char *cat(char *path, const char *directory)
{
    char *buffer = malloc(strlen(path) + strlen(directory) + 2);

    if (buffer == NULL)
        return NULL;

    strcpy(buffer, path);

    strcat(buffer, " ");

    return strcat(buffer, directory);
}

char *nonspace(char *path, const char *directory)
{
    char *buffer = malloc(strlen(path) + strlen(directory) + 2);

    if (buffer == NULL)
        return NULL;

    strcpy(buffer, path);

    strcat(buffer, "");

    return strcat(buffer, directory);
}

int main() 
{

	yyparse();	
	return 0;
}

void yyerror(const char *s) {

	printf("Code Java incorrect à la ligne : %d\n",yylineno);
}

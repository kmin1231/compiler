%{
module A = Absyn
module S = Symbol

let start_pos = Parsing.symbol_start
let end_pos = Parsing.symbol_end
%}

%start prog

%token COMMA SEMICOLON COLON
%token LPAREN RPAREN
%token PLUS MINUS TIMES
%token LT EQ GT
%token AND NOT OR
%token WHILE DO REF BANG ASSIGN
%token IF THEN ELSE
%token LET IN FUN ARROW TYPE UMINUS
%token EOF
%token <string> ID
%token <int> NUM PROJ
%token GET SET TUPLE INT

%type <Absyn.prog> prog
%type <Absyn.fundec list> fundec_list
%type <Absyn.fundec> fundec
%type <Absyn.func> func

%type <Absyn.exp> exp
%type <Absyn.tp> tp
%type <Absyn.tp list> tp_list
%type <Absyn.exp list> exp_list

%left LET IN
%left SEMICOLON
%left THEN
%left IF ELSE DO WHILE
%left ASSIGN
%left COLON
%left AND OR
%left NOT
%left EQ LT
%left PLUS MINUS
%right ARROW
%left TIMES
%left PROJ REF BANG UMINUS LPAREN

%%

prog: fundec_list { $1 }

fundec_list:
  | fundec fundec_list { $1 :: $2 }
  | { [] }         /* empty list */

fundec: func {(start_pos(), end_pos()), $1}

func:
  | FUN ID LPAREN ID COLON tp RPAREN COLON tp EQ exp { 
      (S.symbol $2, S.symbol $4, $6, $9, $11)
    }

tp:
  | ID { if $1 = "int" then A.Inttp else raise (Parsing.Parse_error) }
  | NUM                   { A.Inttp }              /* no argument */
  | LT GT                 { A.Tupletp( [] ) }      /* n=0 for n-ary tuple */
  | LT tp_list GT         { A.Tupletp( $2 ) }      /* tuple expression */
  | tp REF                { A.Reftp($1) }
  | tp ARROW tp           { A.Arrowtp ($1, $3) }
  | LPAREN tp RPAREN      { $2 }

tp_list:
  | tp { [$1] }
  | tp COMMA tp_list { $1 :: $3 }
  | { [] }        /* empty list */    

exp_list:
  | exp { [$1] }
  | exp COMMA exp_list { $1 :: $3 }
  | { [] }        /* empty list */

exp:
  | ID { A.Pos((start_pos(), end_pos()), A.Id(S.symbol $1)) }    
  | NUM { A.Pos((start_pos(), end_pos()), A.Int($1)) }         
  | LT exp_list GT { A.Pos((start_pos(), end_pos()), A.Tuple($2)) }

  | LPAREN exp RPAREN { A.Pos((start_pos(), end_pos()), $2) }        
  
  | exp SEMICOLON exp { A.Pos((start_pos(), end_pos()), A.Let(S.symbol "", $1, $3)) }
  | MINUS exp %prec UMINUS { A.Pos((start_pos(), end_pos()), A.Op(A.Sub, [A.Int(0); $2])) }
  | NOT exp { A.Pos((start_pos(), end_pos()), A.Op(A.Sub, [A.Int(1); $2])) }
  | BANG exp { A.Pos((start_pos(), end_pos()), A.Op(A.Get, [$2])) }   

  | PROJ exp { A.Pos((start_pos(), end_pos()), A.Proj($1, $2)) }
/*  | PROJ NUM exp { A.Proj($2, $3) } */

  | exp PLUS exp { A.Pos((start_pos(), end_pos()), A.Op(A.Add, [$1; $3])) }
  | exp MINUS exp { A.Pos((start_pos(), end_pos()), A.Op(A.Sub, [$1; $3])) }
  | exp TIMES exp { A.Pos((start_pos(), end_pos()), A.Op(A.Mul, [$1; $3])) }
  | exp AND exp { A.Pos((start_pos(), end_pos()), A.If($1, $3, A.Int(0))) }
  | exp OR exp { A.Pos((start_pos(), end_pos()), A.If($1, A.Int(1), $3)) }
  | exp LT exp { A.Pos((start_pos(), end_pos()), A.Op(A.LT, [$1; $3])) }
  | exp EQ exp { A.Pos((start_pos(), end_pos()), A.Op(A.Eq, [$1; $3])) }
/*  | exp GT exp { A.Pos((start_pos(), end_pos()), A.Op(A.LT, [$3; $1])) } */
  | exp REF exp { A.Pos((start_pos(), end_pos()), A.Op(A.Ref, [$1; $3])) }

  | exp LPAREN exp RPAREN { A.Pos((start_pos(), end_pos()), A.Call($1, $3)) }
  
  | exp COLON tp { A.Pos((start_pos(), end_pos()), A.Constrain($1, $3)) }
  | IF exp THEN exp ELSE exp { A.Pos((start_pos(), end_pos()), If($2, $4, $6)) }
  | IF exp THEN exp { A.Pos((start_pos(), end_pos()), If($2, $4, A.Int(0))) }
  | WHILE exp DO exp { A.Pos((start_pos(), end_pos()), A.While($2, $4)) }
  | LET ID EQ exp IN exp { A.Pos((start_pos(), end_pos()), A.Let(S.symbol $2, $4, $6)) }
  | REF exp { A.Pos((start_pos(), end_pos()), A.Op(A.Ref, [$2])) }
  | exp ASSIGN exp { A.Pos((start_pos(), end_pos()), A.Op(A.Set, [$1; $3])) }

%%

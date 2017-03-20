%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA ASSIGN BAR COLON QUOTES QUESMARK DOT LSQUARE RSQUARE
%token ADD SUB MUL DIV
%token EQ NEQ LT LE GT GE AND OR NEG NOT
%token RETURN IF ELSE ELIF FOR WHILE IN
%token INT BOOL VOID CHAR STRING
%token CONTINUE BREAK
%token EOF

(* tokens specific to our language *)
%token TYPE SWITCH CASE GOTO FSM STATE START INPUT OUTPUT SYSIN
%token CREATE SIM REACH TICK RESET

(* literals *)
%token <int> INTLIT
%token <bool> BOOLIT
%token <char> CHARLIT
%token <string> STRINGLIT
%token <string> ID

%start expr
%type < Ast.expr> expr

%%
(*grammar *)
dtype:
BOOL { Bool($1) }
| INT { Int($1) }
| Char { CHAR($1) }
| dtype LSQUARE INTLIT RSQUARE { Array($1, $3) }
| ID { Enum($1) }  (* Q: Not sure if this is correct *)

lvalue:
dtype ID { $1, $2 }

literal:
INTLIT { IntLit($1) }
| BOOLIT { BoolLit($1) }
| CHARLIT { CharLit($1) }
| INTLIT COLON INTLIT COLON INTLIT { Range($1, $3, $5) } (* Q: wouldn't this apply only to integers? *)
| literal_opt { ArrayLit($1) } (* see list definitions below *)

expr:
literal { Literal($1) }
| ID { Variable($1) }
| NEG expr { Uop(Neg, $2)}
| NOT expr { Uop(Not, $2)}
| expr PLUS   expr { Binop($1, Add, $3) }
| expr MINUS  expr { Binop($1, Sub, $3) }
| expr TIMES  expr { Binop($1, Mul, $3) }
| expr DIVIDE expr { Binop($1, Div, $3) }
| expr EQ expr { Binop($1, Eq, $3) }
| expr NEQ expr { Binop($1, Neq, $3) }
| expr LT expr { Binop($1, Lt, $3) }
| expr LE expr { Binop($1, Le, $3) }
| expr GT expr { Binop($1, Gt, $3) }
| expr GE expr { Binop($1, Ge, $3) }
| expr AND expr { Binop($1, And, $3) }
| expr OR expr { Binop($1, Or, $3) }
| ID ASSIGN expr { Assign($1, $3) }
| ID LPAREN expr_list RPAREN { call($1, $3) }
| ID DOT CREATE LPAREN expr_opt RPAREN { Fsm_call($1, Create, $5) } (* Q: When did we define create? What doth this do? *)
| ID DOT SIM LPAREN expr_opt RPAREN { Fsm_call($1, Sim, $5) }
| ID DOT REACH LPAREN expr_opt RPAREN { Fsm_call($1, Reach, $5) }
| ID DOT TICK LPAREN expr_opt RPAREN { Fsm_call($1, Tick, $5) }
| ID DOT RESET LPAREN expr_opt RPAREN { Fsm_call($1, Reset, $5) } (* Q: there shouldn't be anything in here. Potential for error *)
| expr QUESMARK expr COLON expr { Cond($1, $3, $5) }
| (* nothing *)

stmt:
LBRACE stmt_list RBRACE { Block(List.rev $2) }
| IF expr LBRACE stmt RBRACE stmt { If($2, $4, $6) }  (* no else or elif *)
| IF expr LBRACE stmt RBRACE ELSE stmt { If($2, $4, $7) }  (* with else *)
| IF expr LBRACE stmt RBRACE ELIF stmt { If($2, $4, $7) }  (* with elif *)
| FOR ID IN LPAREN expr RPAREN LBRACE stmt RBRACE { For($2, $5, $8) }
| WHILE LPAREN expr RPAREN LBRACE stmt RBRACE { While($3, $6) }
| expr { Expr($1) }
| GOTO ID { Goto ($2) } (* DO SWITCH LATER *)
| RETURN expr { Return($2) }

type_decl:
  TYPE ID ASSIGN string_opt  (* should these be strings? *)
  {{
    name = $2
    types = $4
  }}

state_decl:
  START ID LBRACE stmt_list RBRACE (* Q: confused about the bool part for START *)
  {{
    name = $2
    start = true (* Q: pretty sure this is wrong :) *)
    body = List.rev $4
  }}
| ID LBRACE stmt_list RBRACE
  {{
    name = $1
    start = false (* Q: probably wrong *)
    body = List.rev $3
  }}

fsm_decl:
  FSM ID LBRACE lvalue_opt INPUT lvalue_opt OUTPUT lvalue_opt state_list RBRACE  (* Q:What about statements? *)
  {{
    name = $2
    locals = $4
    input = $5
    output = $6
    body = List.rev $7
  }}

func_decl:
  dtype ID LPAREN lvalue_opt RPAREN LBRACE lvalue_opt stmt_list RBRACE
  {{
    return = $1
    name = $2
    formals = $4
    locals = $7
    body = List.rev $8
  }}

program:
  type_list fsm_list func_list
  {{
    types = List.rev $1
    fsms = List.rev $2
    funcs = List.rev $3
  }}


(* list definitions*)
expr_opt:
  /* nothing */ { [] }
| expr_list { List.rev $1 }

expr_list:
  expr { [$1] }
| expr_list COMMA expr { $3 :: $1}

literal_opt:
  /* nothing */ { [] }
| literal_list { List.rev $1 }

literal_list:
  literal { $1 }
| literal_list BAR literal { $3 :: $1 }

stmt_list:
  /* nothing */ { [] }
| stmt_list stmt { $2 :: $1 }

string_opt:
  /* nothing */ { [] }
| string_list { List.rev $1 }

string_list:
  STRINGLIT { $1 }
| string_list BAR STRINGLIT { $3 :: $1 }

lvalue_opt:
  /*nothing*/ { [] }
| LSQUARE lvalue_list RSQUARE { List.rev $2 }

lvalue_list:
  lvalue { [$1] }
| lvalue_list COMMA lvalue { $3 :: $1 }

state_list:
/* nothing */ { [] }
| state_list state_decl { $2 :: $1}

type_list:
/* nothing */ { [] }
| type_list type_decl { $2 :: $1}

fsm_list:
/* nothing */ { [] }
| fsm_list fsm_decl { $2 :: $1}

func_list:
/* nothing */ { [] }
| func_list func_decl { $2 :: $1}

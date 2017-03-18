%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA ASSIGN BAR COLON QUOTES QUESMARK DOT
%token ADD SUB MUL DIV
%token EQ NEQ LT LE GT GE AND OR NEG NOT
%token RETURN IF ELSE FOR WHILE
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
BOOL RBRACE INTLIT LBRACE { Array($1, $3) }
| INT RBRACE INTLIT LBRACE { Array($1, $3) }
| CHAR RBRACE INTLIT LBRACE { Array($1, $3) }
| dtype RBRACE INTLIT LBRACE { Array($1, $3) }
| ID { Enum($1) }  (*Q: Not sure if this is correct *)

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
| ID DOT CREATE LPAREN expr_list RPAREN { Fsm_call($1, $3, $5) } (* When did we define create? What doth this do? *)
| ID DOT SIM LPAREN expr_list RPAREN { Fsm_call($1, $3, $5) }
| ID DOT REACH LPAREN expr_list RPAREN { Fsm_call($1, $3, $5) }
| ID DOT TICK LPAREN expr_list RPAREN { Fsm_call($1, $3, $5) }
| ID DOT RESET LPAREN expr_list RPAREN { Fsm_call($1, $3, $5) } (* Q: there shouldn't be anything in here. Potential for error *)
| expr QUESMARK expr COLON expr { Cond($1, $3, $5) }

(* list definitions *)
expr_opt:
  /*nothing*/ { [] }
| expr_list { List.rev $1 }

expr_list:
  expr { [$1] }
| expr_list COMMA expr { $3 :: $1}

literal_opt:
  /*nothing*/ { [] }
| literal_list { List.rev $1 }

literal_list:
  literal { [$1] }
| literal_list BAR literal { $3 :: $1 }

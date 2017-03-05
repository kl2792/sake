%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA ASSIGN BAR
%token ADD SUB MUL DIV
%token EQ NEQ LT LE GT GE AND OR NEG NOT
%token RETURN IF ELSE FOR WHILE
%token INT BOOL VOID CHAR STRING
%token CONTINUE BREAK
%token EOF

%token TYPE SWITCH CASE GOTO FSM STATE START INPUT OUTPUT SYSIN

(* literals *)
%token <int> INTLIT
%token <bool> BOOLIT
%token <char> CHARLIT
%token <string> STRINGLIT

%start expr
%type < Ast.expr> expr

%%
literal:
 literal literal literal { Range($1, $2, $3) }

utype:


expr:
 INTLIT { Literal($1) }
| BOOLIT { Literal($1) }
| CHARLIT { Literal($1) }
| STRINGLIT { Literal($1) } (* confused about range and array *)

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

| STRINGLIT ASSIGN expr { Assign($1, $3) }

| expr expr expr { Tern($1, $2, $3) }

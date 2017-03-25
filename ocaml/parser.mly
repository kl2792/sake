%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA ASSIGN BAR COLON QUOTES QUESMARK DOT LSQUARE RSQUARE NLINE
%token ADD SUB MUL DIV
%token EQ NEQ LT LE GT GE AND OR NEG NOT TRUE FALSE
%token RETURN IF ELSE ELIF FOR WHILE IN
%token INT BOOL VOID CHAR STRING
%token CONTINUE BREAK
%token EOF

/*tokens specific to our language */
%token TYPE SWITCH CASE GOTO FSM STATE START INPUT OUTPUT SYSIN
%token TICK RESET

/* ASSOCIATIVITY */
%nonassoc NOELSE
%nonassoc ELSE
%left RETURN
%left COMMA
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LE GE
%left ADD SUB
%left MUL DIV
%right NOT NEG

/*literals */
%token <int> INTLIT
%token <int> RANGELEM
%token <char> CHARLIT
%token <string> STRINGLIT
%token <string> ID /*why string? */
%token <string> TYPENAME

%start program
%type <Ast.program> program

%%
/* grammar */
dtype:
BOOL { Bool }
| INT { Int }
| CHAR { Char }
| dtype LSQUARE INTLIT RSQUARE { Array($1, $3) }  /*??  ast as well */
| ID { Enum($1) }  /*Q: Not sure if this is correct */

lvalue:
 dtype ID { $1, $2 }

literal:
INTLIT { IntLit($1) }
| TRUE { BoolLit(true) }
| FALSE { BoolLit(false) }
| CHARLIT { CharLit($1) }
// DON'T NEED FOR HELLO WORLD | INTLIT COLON INTLIT COLON INTLIT { Range($1, $3, $5) }
// DON'T NEED FOR HELLO WORLD | literal_opt { ArrayLit($1) } /*see list definitions below */

expr:
literal { Literal($1) }
| ID { Variable($1) }
| SUB expr %prec NEG { Uop(Neg, $2) }
| NOT expr { Uop(Not, $2) }
| expr ADD expr { Binop($1, Add, $3) }
| expr SUB expr { Binop($1, Sub, $3) }
| expr MUL expr { Binop($1, Mul, $3) }
| expr DIV expr { Binop($1, Div, $3) }
| expr EQ expr { Binop($1, Eq, $3) }
| expr NEQ expr { Binop($1, Neq, $3) }
| expr LT expr { Binop($1, Lt, $3) }
| expr LE expr { Binop($1, Le, $3) }
| expr GT expr { Binop($1, Gt, $3) }
| expr GE expr { Binop($1, Ge, $3) }
| expr AND expr { Binop($1, And, $3) }
| expr OR expr { Binop($1, Or, $3) }
| ID ASSIGN expr { Assign($1, $3) }
| ID LPAREN actuals_opt RPAREN { Call($1, $3) }
| ID DOT TICK LPAREN actuals_opt RPAREN { Fsm_call($1, Tick, $5) }
| ID DOT RESET LPAREN actuals_opt RPAREN { Fsm_call($1, Reset, $5) }  /*Q: there shouldn't be anything in here. Potential for error */
// Can solve with Associativity | expr QUESMARK expr COLON expr { Cond($1, $3, $5) }

case:
CASE expr COLON { CaseValue($2) }
| /* nothing */ { CaseAny }

stmt:
LBRACE stmt_list RBRACE NLINE { Block(List.rev $2) }
| IF expr LBRACE stmt RBRACE %prec NOELSE { If($2, $4, Block([])) }  /*no else or elif */ /*is this needed? */
| IF expr LBRACE stmt RBRACE ELSE stmt { If($2, $4, $7) }  /*with else */
// Kind of Jank | IF expr LBRACE stmt RBRACE ELIF stmt { If($2, $4, $7) }  /*with elif */
| FOR ID IN LPAREN expr RPAREN LBRACE stmt RBRACE { For($2, $5, $8) }
| WHILE LPAREN expr RPAREN LBRACE stmt RBRACE { While($3, $6) }
| expr NLINE{ Expr($1) }
| SWITCH LPAREN expr RPAREN LBRACE cstmt_list RBRACE { Switch($3, List.rev $6) }
| GOTO ID NLINE { Goto ($2) }
| RETURN expr NLINE { Return($2) }

 type_decl:
  TYPE ID ASSIGN string_opt NLINE
  {{
    name = $2;
    types = $4;
  }}

state_decl:
  START ID LBRACE stmt_list RBRACE
  {{
    name = $2;
    start = true;
    body = List.rev $4;
  }}
| ID LBRACE stmt_list RBRACE
  {{
    name = $1;
    start = false;
    body = List.rev $3;
  }}

fsm_decl:
  FSM ID LBRACE lvalue_opt INPUT LSQUARE lvalue_opt RSQUARE OUTPUT LSQUARE lvalue_opt RSQUARE state_list RBRACE  /*Q:What about statements? */
  {{
    name = $2;
    locals = $4;
    input = $7;
    output = $11;
    body = List.rev $13;
  }}
| FSM ID LBRACE lvalue_opt OUTPUT LSQUARE lvalue_opt RSQUARE state_list RBRACE  /*Q:What about statements? */
  {{
    name = $2;
    locals = $4;
    input = [];
    output = $7;
    body = List.rev $9;
  }}
| FSM ID LBRACE lvalue_opt state_list RBRACE  /*Q:What about statements? */
  {{
    name = $2;
    locals = $4;
    input = [];
    output = [];
    body = List.rev $5;
  }}
| FSM ID LBRACE lvalue_opt INPUT LSQUARE lvalue_opt RSQUARE state_list RBRACE  /*Q:What about statements? */
{{
  name = $2;
  locals = $4;
  input = [];
  output = $7;
  body = List.rev $9;
}}


 func_decl:
  dtype ID LPAREN lvalue_opt RPAREN LBRACE lvalue_opt stmt_list RBRACE
  {{
    return = $1;
    name = $2;
    formals = $4;
    locals = $7;
    body = List.rev $9;
  }}

 program:
  type_list fsm_list func_list EOF
  {{
    types = List.rev $1;
    fsms = List.rev $2;
    funcs = List.rev $3;
  }}


/*list definitions */
//expr_opt:
//  /* nothing */ { Empty }
//| expr { $1 }

actuals_opt:
  /* nothing */ { [] }
| actuals_list { List.rev $1 }

actuals_list:
  expr { [$1] }
| actuals_list COMMA expr { $3 :: $1}

literal_opt:
  /* nothing */ { [] }
| literal_list { List.rev $1 }

literal_list:
  literal { [$1] }
| literal_list COMMA literal { $3 :: $1 }

stmt_list:
  /* nothing */ { [] }
| stmt_list stmt { $2 :: $1 }

cstmt_list:
/* nothing */ { [] }
| cstmt_list case stmt {  ($2, $3) :: $1 } /* Q: a little confused how to go about this */

string_opt:
  /* nothing */ { [] }
| string_list { List.rev $1 }

string_list:
  TYPENAME { [$1] }
| string_list BAR TYPENAME { $3 :: $1 }

lvalue_opt:
  /*nothing*/ { [] }
| lvalue_list { List.rev $1 }

lvalue_list:
  lvalue { [$1] }
| lvalue_list COMMA lvalue { $3 :: $1 }

lvalue_opt_two:


lvalue_list_two:
   lvalue NLINE { [$1] }
| lvalue_list lvalue

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

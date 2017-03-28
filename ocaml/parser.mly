%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA ASSIGN BAR COLON QUOTES QUESMARK DOT LSQUARE RSQUARE NLINE UNDER
%token ADD SUB MUL DIV
%token EQ NEQ LT LE GT GE AND OR NEG NOT TRUE FALSE
%token RETURN IF ELSE ELIF FOR WHILE IN
%token INT BOOL VOID CHAR STRING
%token CONTINUE BREAK
%token EOF

/*tokens specific to our language */
%token TYPE SWITCH CASE GOTO FSM STATE START INPUT OUTPUT SYSIN
%token TICK RESET PRINT

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
// %token <string> STRINGLIT
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

expr:
INTLIT { IntLit($1) }
| TRUE { BoolLit(true) }
| FALSE { BoolLit(false) }
| CHARLIT { CharLit($1) }
// DON'T NEED FOR HELLO WORLD | INTLIT COLON INTLIT COLON INTLIT { Range($1, $3, $5) }
//| { ArrayLit($1) } /*see list definitions below */
| QUOTES char_opt QUOTES { StringLit(List.rev $2)} // array of characters (string)
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
| ID UNDER TICK LPAREN actuals_opt RPAREN { Fsm_call($1, Tick, $5) }
| PRINT LPAREN expr RPAREN { Print($3) } //last minute
// Can solve with Associativity | expr QUESMARK expr COLON expr { Cond($1, $3, $5) }

case:
CASE expr { CaseValue($2) }
| /* nothing */ { CaseAny }

stmt:
LBRACE stmt_list2 RBRACE NLINE { Block(List.rev $2) }
| IF LPAREN expr RPAREN LBRACE NLINE stmt RBRACE NLINE %prec NOELSE { If($3, $7, Block([])) }  /*no else or elif */ /*is this needed? */
| IF LPAREN expr RPAREN LBRACE NLINE stmt RBRACE NLINE ELSE NLINE stmt { If($3, $7, $12) }  /*with else */
// Kind of Jank | IF expr LBRACE stmt RBRACE ELIF stmt { If($2, $4, $7) }  /*with elif */
| FOR ID IN LPAREN expr RPAREN LBRACE NLINE stmt RBRACE { For($2, $5, $9) }
| WHILE LPAREN expr RPAREN LBRACE NLINE stmt RBRACE { While($3, $7) }
| expr NLINE{ Expr($1) }
| SWITCH LPAREN expr RPAREN LBRACE NLINE cstmt_list RBRACE { Switch($3, List.rev $7) }
| GOTO ID NLINE { Goto ($2) }
// NOT DOING FUNCTION DECLS | RETURN expr NLINE { Return($2) }

cstmt:
  case COLON stmt {$1, $3}

 type_decl:
  TYPE ID ASSIGN string_opt NLINE
  {{
    type_name = $2;
    type_values = $4;
  }}

state_decl:
  START ID LBRACE stmt_list2 RBRACE
  {{
    state_name = $2;
    state_start = true;
    state_body = List.rev $4;
  }}
| ID LBRACE stmt_list2 RBRACE
  {{
    state_name = $1;
    state_start = false;
    state_body = List.rev $3;
  }}

/* fsm_decl:
  FSM ID LBRACE state_list NLINE RBRACE
{{
  fsm_name = $2;
  fsm_body = List.rev $4;
}} */

// how the mighty have fallen
fsm_decl:
  FSM ID LBRACE stmt_list2 RBRACE
{{
  fsm_name = $2;
  fsm_body = List.rev $5;
}}

 /* func_decl:
  dtype ID LPAREN lvalue_opt RPAREN LBRACE lvalue_list2 stmt_list2 RBRACE
  {{
    return = $1;
    name = $2;
    formals = $4;
    locals = List.rev $7;
    body = List.rev $8;
  }}
| dtype ID LPAREN lvalue_opt RPAREN LBRACE stmt_list2 RBRACE
{{
  return = $1;
  name = $2;
  formals = $4;
  locals = [];
  body = List.rev $7;
}} */

program:
/* Bug: if no type_list need NLINE NLINE */
  INPUT LSQUARE lvalue_list RSQUARE NLINE OUTPUT LSQUARE lvalue_list RSQUARE lvalue_list2 type_list NLINE fsm_list EOF
  {{
    input = List.rev $3;
    output = List.rev $8;
    locals = List.rev $10;
    types = List.rev $11;
    fsms = List.rev $13;
  }}
/* MAXIMUM JANKNESS */
| fsm_list EOF
{{
  input = [];
  output = [];
  locals = [];
  types = [];
  fsms = List.rev $1;
}}


/*list definitions */
char_opt:
  /* nothing */ { [] }
| char_opt CHARLIT { $2 :: $1 }

actuals_opt:
  /* nothing */ { [] }
| actuals_list { List.rev $1 }

actuals_list:
  expr { [$1] }
| actuals_list COMMA expr { $3 :: $1}

stmt_list:
  /* nothing */ { [] }
| stmt_list stmt { $2 :: $1 }

stmt_list2:
  NLINE { [] }
| stmt_list2 stmt { $2 :: $1 }

cstmt_list:
/* nothing */ { [] }
| cstmt_list NLINE cstmt {  $3 :: $1 }

string_opt:
  /* nothing */ { [] }
| string_list { List.rev $1 }

string_list:
  TYPENAME { [$1] }
| string_list BAR TYPENAME { $3 :: $1 }

lvalue_opt:
  NLINE { [] }  /* So it's not empty */
| lvalue_list { List.rev $1 }

lvalue_list:
  lvalue { [$1] }
| lvalue_list COMMA lvalue { $3 :: $1 }

lvalue_list2: /* alternative way to list lvalues, line by line */
   NLINE { [] }
| lvalue_list NLINE lvalue { $3 :: $1 }

state_list:
/* nothing */ { [] }
| state_list NLINE state_decl { $3 :: $1}

type_list:
/* nothing */ { [] }
| type_list type_decl { $2 :: $1}

fsm_list:
/* nothing */ { [] }
| fsm_list fsm_decl { $2 :: $1}

//func_list:
///* nothing */ { [] }
//| func_list func_decl { $2 :: $1}

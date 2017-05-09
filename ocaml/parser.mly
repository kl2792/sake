%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA ASSIGN BAR COLON QUOTES DOT LSQUARE RSQUARE NLINE UNDER
%token ADD SUB MUL DIV
%token EQ NEQ LT LE GT GE AND OR NEG NOT TRUE FALSE
%token IF ELSE FOR WHILE IN
%token INT BOOL VOID CHAR STRING
%token EOF

/* tokens specific to our language */
%token TYPE SWITCH CASE GOTO FSM STATE START INPUT OUTPUT PUBLIC
%token PRINTF HALT

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

/* literals */
%token <int> INTLIT
%token <char> CHARLIT
%token <string> STRINGLIT
%token <string> ESCAPE
%token <string> ID
%token <string> TYPENAME

%start program
%type <Ast.program> program

%%
/* grammar */
dtype:
| BOOL { Bool }
| INT { Int }
| CHAR { Char }
| STRING { String }
| TYPENAME { Enum($1) }  

lvalue:
 dtype ID { $1, $2 }

/* expressions */
expr:
| INTLIT { IntLit($1) }
| TRUE { BoolLit(true) }
| FALSE { BoolLit(false) }
| CHARLIT { CharLit($1) }
| STRINGLIT { StringLit($1) }
| ID { Variable($1) }
| TYPENAME { EnumLit($1) }
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
| PRINTF LPAREN STRINGLIT COMMA actuals_list RPAREN { Printf($3, List.rev $5) }
| PRINTF LPAREN ESCAPE COMMA actuals_list RPAREN { Printf($3 ^ "\n", List.rev $5) }
| ID DOT ID { Access($1, $3) }

/* statements */
stmt:
| LBRACE NLINE stmt_list RBRACE NLINE { Block(List.rev $3) }
| STATE TYPENAME NLINE { State($2) }
| IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }  
| IF LPAREN expr RPAREN stmt ELSE stmt { If($3, $5, $7) }  /* with else */
| FOR ID IN LPAREN INTLIT COLON INTLIT COLON INTLIT RPAREN stmt { For($2, $5, $7, $9, $11) }  
| WHILE LPAREN expr RPAREN stmt { While($3, $5) }
| expr NLINE { Expr($1) }
| SWITCH LPAREN expr RPAREN LBRACE cstmt_list RBRACE NLINE { Switch($3, List.rev $6) }
| GOTO TYPENAME NLINE { Goto ($2) }
| HALT NLINE { Halt }

stexpr:
ID expr { $1, $2 }

cstmt:
  CASE expr COLON stmt_list { $2, List.rev $4 }

type_decl:
  TYPE TYPENAME ASSIGN string_opt 
  {{
    type_name = $2;
    type_values = $4;
  }}

fsm_decl:
  FSM ID LBRACE NLINE public_opt local_opt NLINE stmt_list RBRACE NLINE
{{
  fsm_name = $2;
  fsm_public = List.rev $5;
  fsm_locals = List.rev $6;
  fsm_body = List.rev $8;
}} 

program:
| INPUT LSQUARE lvalue_list RSQUARE NLINE
  OUTPUT LSQUARE lvalue_list RSQUARE NLINE NLINE
  type_opt fsm_list EOF
  {{
    input = List.rev $3;
    output = List.rev $8;
    types = $12;
    fsms = List.rev $13;
  }}
| type_opt fsm_list EOF
  {{
    input = [];
    output = [];
    types = $1;
    fsms = List.rev $2;
  }}


/* list definitions */
actuals_list:
| expr { [$1] }
| actuals_list COMMA expr { $3 :: $1} 

stexpr_list:
| stexpr { [$1] }
| stexpr_list COMMA stexpr { $3 :: $1}

stmt_list:
| /* nothing */ { [] }
| stmt_list stmt { $2 :: $1 }

cstmt_list: 
| NLINE { [] }
| cstmt_list cstmt {  $2 :: $1 }

string_opt:
| /* nothing */ { [] }
| string_list { List.rev $1 }

string_list:
| TYPENAME { [$1] }
| string_list BAR TYPENAME { $3 :: $1 }

lvalue_list:
| lvalue { [$1] }
| lvalue_list COMMA lvalue { $3 :: $1 }

dstexpr:
| dtype ID ASSIGN expr { $1, $2, $4 }
| dtype ID { $1, $2, Empty }

public_opt:
| /* nothing */ { [] }
| public_list NLINE { List.rev $1 }

public_list:
| PUBLIC dstexpr { [$2] }
| public_list NLINE PUBLIC dstexpr { $4 :: $1 }

local_opt:
| /* nothing */ { [] }
| local_list NLINE { List.rev $1 }

local_list:
| dstexpr { [$1] }
| local_list NLINE dstexpr { $3 :: $1 }

type_opt:
| /* nothing */ { [] }
| type_list NLINE NLINE { List.rev $1 }

type_list:
| type_decl { [$1] }
| type_list type_decl { $2 :: $1 }

fsm_list:
| /* nothing */ { [] }
| fsm_list fsm_decl { $2 :: $1 }

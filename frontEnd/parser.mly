// terminants
%token LPAREN RPAREN
// %token LBRACK RBRACK     !TODO
// %token COMMENT_HASH      --this will be ignored in the lexer
%token DEFINE END COMMA OR_FORMULAE OFCOURSE SKIP NOT_FORMULAE
%left OR_FORMULAE           // could also be %right, but it doesn't matter
%left COMMA
%left NOT_FORMULAE
%token <string> ATOM VARIABLE
%token <int> ATOM_INT
%token GOAL_INIT

%start program
%type <Ast.statement> program
%type <Ast.statement> stmt
%type <Ast.goal_ast> body
%type <Ast.clause_ast> clause

// non-terminants

%%
program:
    | stmt             { $1 }
;

stmt:
    | goal        {Ast.GoalStatement $1}
    | clause      {Ast.ClauseStatement $1}
;

goal:
    | GOAL_INIT body END {$2}
;

clause:
    | head DEFINE body END 
        { {head=$1; body= Some $3}}
    | head END 
        { {head=$1; body= None}}
;

body:
    | term                           {Ast.Atomic $1}
    | OFCOURSE                       {Ast.OfCourse}
    | LPAREN body RPAREN             {$2}
    | body COMMA body                {Ast.And ($1, $3)}
    | body OR_FORMULAE body          {Ast.Or ($1, $3)}
    | NOT_FORMULAE body              {Ast.Not $2}
;  

head:
    | ATOM LPAREN head_terms RPAREN  {Ast.K'ary {symbol=$1; args=$3}}
head_terms:
    | empty                         { [] }
    | head_term                     { [$1]}
    | head_term COMMA head_terms     { $1 :: $3 }
head_term:
    | VARIABLE                      {Ast.Var $1}
    | ATOM                          {Ast.Const_atom $1}
    | ATOM_INT                      {Ast.Const_int $1}
    | head                          {$1}
    | SKIP                          {Ast.Skip}
;

term:
    | ATOM                        {Ast.Const_atom $1}
    | VARIABLE                    {Ast.Var $1}
    | ATOM_INT                    {Ast.Const_int $1}
    | k_ary                       {$1}
;
k_ary:
    | ATOM LPAREN terms RPAREN    {Ast.K'ary {symbol=$1; args=$3}}
;
terms:
    | empty                       { [] }
    | term                        { [$1]}
    | term COMMA terms            { $1 :: $3 }
;
empty:
    |                            { [] }
;
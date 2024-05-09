{
    open Parser
}

let whitespace = [' ''\t''\n']
let alphanumerics = ['A'-'Z' 'a'-'z' '0'-'9']


rule logprog_parser = parse
    | whitespace+ { logprog_parser lexbuf }
    | '#'[^'\n']* { logprog_parser lexbuf } (* comments *)
    | '(' {LPAREN}
    | ')' {RPAREN}
    | ',' {COMMA}
    | '.' {END}
    | '!' {OFCOURSE}
    | ';' {OR_FORMULAE}
    | '_' {SKIP}
    | '?' whitespace* '-' {GOAL_INIT}
    | ':'whitespace*'-' {DEFINE}
    | '/'whitespace*'+' {NOT_FORMULAE}
    | ('+'|'-')?['0'-'9']+ as i {ATOM_INT(int_of_string i)}
    | ['A'-'Z']alphanumerics* as s {VARIABLE s}
    | ['a'-'z']alphanumerics* as s {ATOM s}
    | ''' ([^'\n' ''']+ as s) '''  {ATOM s}
    | '"' ([^'\n''"']+ as s) '"'   {ATOM s}

    | _  {raise (Failure ("illegal character '" ^ Lexing.lexeme lexbuf ^ "' while lexing"))}
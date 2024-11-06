{
  open Parser
}

let white = [' ' '\t']+
let num = ['0'-'9']|['1'-'9']['0'-'9']*
let exa_num = ['0']['X' 'x']['0'-'9' 'A'-'F' 'a'-'f']*

rule read_token =
  parse
  | white { read_token lexbuf }  
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { STAR }
  | "/" { SLASH }
  | num { CONST (Lexing.lexeme lexbuf) }
  | exa_num { CONST (Lexing.lexeme lexbuf) }
  | eof { EOF }

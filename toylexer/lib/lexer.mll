{
  open Token
}

let white = [' ' '\t']+
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let chr = ['a'-'z' 'A'-'Z' '0'-'9']
let id = letter chr*
let num = ['0'-'9']|['1'-'9']['0'-'9']*

let vowel = ['a' 'e' 'i' 'o' 'u']
let consonant = ['b'-'d' 'f'-'h' 'j'-'n' 'p'-'y' 'v'-'z' 'B'-'D' 'F'-'H' 'J'-'N' 'P'-'Y' 'V'-'Z']

let A = ['A'-'Z'] chr*
let B = vowel+
let C = consonant* vowel? consonant*
let D = ['-']? digit+ ['.']? digit*
let E = ['0'] ['X' 'x'] ['0'-'9' 'A'-'F' 'a'-'f']+

rule read_token =
  parse
  | white { read_token lexbuf }  
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "=" { ASSIGN }
  | "+" { PLUS }
  | ";" { SEQ }  
  | A { ATOK (Lexing.lexeme lexbuf) }  
  | B { BTOK (Lexing.lexeme lexbuf) }  
  | C { CTOK (Lexing.lexeme lexbuf) }  
  | D { DTOK (Lexing.lexeme lexbuf) }
  | E { ETOK (Lexing.lexeme lexbuf) }
  | id { ID (Lexing.lexeme lexbuf) }
  | num { CONST (Lexing.lexeme lexbuf) }    
  | eof { EOF }

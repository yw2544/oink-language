{
    open Parser

    exception SyntaxError of string

    let line_num = ref 1

    let next_line lexbuf = 
        incr line_num;
        Lexing.new_line lexbuf
}

let digit = ['0'-'9']
let int = '-'? digit+

let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit+ '.' digit+ (['e' 'E'] ['-' '+']? digit+)?

let white = [' ' '\t']+
let newline = '\r' | '\n' |"\r\n"
let id = ['a'-'z' 'A'-'Z' '_']+

rule read = 
    parse 
    | white {read lexbuf}
    | newline {next_line lexbuf; read lexbuf}
    | '"' {read_string (Buffer.create 17) lexbuf}
    | "#" {GATE}
    | "baaa" {BAAA}
    | "true" {TRUE}
    | "false" {FALSE}
    | "and" {AND}
    | "or" {OR}
    | "workhorse"{WORKHORSE}
    | "oink" {OINK}
    | "go!"{GO}
    | "=" {EQ}
    | "mud" {MUD}
    | ";" {SEP}
    (*| id { print_endline ("Token: IDENT (" ^ Lexing.lexeme lexbuf ^ ")"); IDENT (Lexing.lexeme lexbuf) }*)
    | id { IDENT (Lexing.lexeme lexbuf) }
    | int { INT (int_of_string (Lexing.lexeme lexbuf )) }
    | float {FLOAT (float_of_string (Lexing.lexeme lexbuf))}
    | eof { EOF }


and read_string buf =
    parse
    | '"' {STRING (Buffer.contents buf)}
    | '\\' '/' {Buffer.add_char buf '/'; read_string buf lexbuf}
    | '\\' '\\' {Buffer.add_char buf '\\'; read_string buf lexbuf}
    | '\\' 'b' {Buffer.add_char buf '\b'; read_string buf lexbuf}
    | '\\' 'f' {Buffer.add_char buf '\012'; read_string buf lexbuf}
    | '\\' 'n' {Buffer.add_char buf '\n'; read_string buf lexbuf}
    | '\\' 'r' {Buffer.add_char buf '\r'; read_string buf lexbuf}
    | '\\' 't' {Buffer.add_char buf '\t'; read_string buf lexbuf}
    | [^ '"' '\\']+ {Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf}
    | _ {raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf))}
    | eof {raise (SyntaxError ("Non-terminating string"))}



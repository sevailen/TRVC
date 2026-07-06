{
    open Parser
}

let whitespace = [' ' '\t' '\r' '\n']

rule read = parse
    | whitespace+ { read lexbuf }
    | "//" { line_comment lexbuf }
    | "/*" { block_comment lexbuf }
    | "int" { INT }
    | "void" { VOID }
    | "const" { CONST }
    | "if" { IF }
    | "else" { ELSE }
    | "while" { WHILE }
    | "break" { BREAK }
    | "continue" { CONTINUE }
    | "return" { RETURN }
    | '+' { PLUS }
    | '-' { MINUS }
    | '*' { STAR }
    | '/' { SLASH }
    | '%' { PERCENT }
    | "<=" { LE }
    | ">=" { GE }
    | "==" { EQ }
    | "!=" { NE }
    | "&&" { AND }
    | "||" { OR }
    | '!' { NOT }
    | '<' { LT }
    | '>' { GT }
    | '=' { ASSIGN }
    | '(' { LPAREN }
    | ')' { RPAREN }
    | '{' { LBRACE }
    | '}' { RBRACE }
    | ';' { SEMICOLON }
    | ',' { COMMA }
    | ['0'-'9']+ as num { NUM (int_of_string num) }
    | ['_' 'a'-'z' 'A'-'Z']['_' 'a'-'z' 'A'-'Z' '0'-'9']* as id { ID id }
    | eof { EOF }
    | _ as c { failwith (Printf.sprintf "Lexical error: unexpected character '%c'" c) }

and line_comment = parse
    | '\n' { read lexbuf }
    | _ { line_comment lexbuf }
    | eof { EOF }

and block_comment = parse
    | "*/" { read lexbuf }
    | _ { block_comment lexbuf }
    | eof { failwith "Unterminated block comment" }

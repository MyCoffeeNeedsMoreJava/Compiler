(**************************************************************************)
(* AU compilation.                                                        *)
(* Skeleton file -- expected to be modified as part of the assignment     *)
(* Do not distribute                                                      *)
(**************************************************************************)

{
  open Tigerparser.Parser  
  exception Error of string
  let error lexbuf msg =
    let position = Lexing.lexeme_start_p lexbuf in
    let err_str = Printf.sprintf "Lexing error in file %s at position %d:%d\n"
                  position.pos_fname position.pos_lnum (position.pos_cnum - position.pos_bol + 1)
                  ^ msg ^ "\n" in
    raise (Error err_str)
}

{
  exception Error of string * Lexing.position

  type token =
  | EOF
  | INT of int
  | ID of string
  | NEWLINE
  | REC
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | REMAINDER
  | ASSIGN
  | ASK
  | TELL
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | DOT
  |
}
let letter=['a'-'z''A'-'Z']
let digit=['0'-'9']

let integer = digit*
let ident = ('_'|letter)('_'|letter|digit)*

(* add more named regexps here *)

(* an entrypoint with a few starting regexps *)
rule token = parse
|  [' ' '\t' ]     { token lexbuf }     (* skip blanks *)
| eof                 { EOF }
| "rec"               { REC }
| "ask"               { ASK }
| "tell"              { TELL }
| integer as i        { INT (int_of_string i) }
| ident as id         { ID id }
| ','                 { COMMA }
| '.'                 { DOT }
| ';'                 { SEMICOLON }
| ":="                { ASSIGN }
| '('                 { RPAREN }
| ')'                 { LPAREN }
| '{'                 { RBRACE }
| '}'                 { LBRACE }
| '*'                 { TIMES }
| '%'                 { REMAINDER }
| '+'                 { PLUS }
| '-'                 { MINUS }
| "array"             { ARRAY }
| "if"                { IF }
| '\n'                { Lexing.new_line texbuf; NEWLINE }
| "//"                { single_line_comment lexbuf }
| "/*"                { multi_line_comment lexbuf }
| '/'                 { DIVIDE }
| digits as i         { INT (int_of_string i) }

(* add your regexps here *)
and single_line_comment = parse
  eof                 { EOF }
| '\n'                { NEWLINE }
| _                   { single_line_comment lexbuf}

and multi_line_comment = parse
  eof                 { raise (Error ("File ended before the comment did \n", lexbuf.lex_curr_p)) }         
| "*/"                { if level = 0 then token lexbuf else multi_line_comment }
| "/*"                { multi_line_comment (level + 1) lexbuf }
| _                   { multi_line_comment level lexbuf }

{

let token_to_string tk = 
    match tk with
    | EOF -> "EOF"
    | INT i -> "INT(" ^ (string_of_int i) ^ ")"
    | ID id -> "ID(" ^ id ^ ")"
    | NEWLINE -> "NEWLINE"
    | REC -> "REC"
    | PLUS -> "PLUS"



}
(* default error handling *)
| _ as t              { error lexbuf ("Invalid character '" ^ (String.make 1 t) ^ "'") }

(**************************************************************************)
(* AU compilation.                                                        *)
(*                                                                        *)
(* Lexer Assignment submissions must not modify this file                 *)
(*                                                                        *)
(*                  Dummy parser for the Lexer assignment                 *)
(**************************************************************************)



%token EOF
%token <string> ID
%token <int> INT 
%token <string> STRING 
%token COMMA COLON SEMICOLON 
%token LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE 
%token DOT PLUS MINUS TIMES DIVIDE EQ NEQ LT LE GT GE 
%token AND OR ASSIGN ARRAY IF THEN ELSE WHILE FOR TO DO
%token LET IN END OF BREAK NIL FUNCTION VAR TYPE CARET 



%start <(string * Lexing.position) list> lexdriver
%%
(* --- lexer driver --- *)

(* only needed for the lexer assignment before student 
   get to implement full parser 
*)

anytoken:
  | id = ID { "ID " ^ id, $startpos } 
  | s = STRING { "STRING \"" ^ String.escaped s ^"\"", $startpos}
  | x = INT { "INT " ^ (string_of_int x) , $startpos}
  | COMMA { "COMMA" , $startpos}
  | COLON { "COLON" , $startpos}  
  | SEMICOLON { "SEMICOLON" , $startpos} 
  | LPAREN { "LPAREN" , $startpos} 
  | RPAREN { "RPAREN" , $startpos} 
  | LBRACK { "LBRACK" , $startpos} 
  | RBRACK { "RBRACK" , $startpos} 
  | LBRACE { "LBRACE" , $startpos} 
  | RBRACE { "RBRACE" , $startpos} 
  | DOT { "DOT" , $startpos} 
  | PLUS { "PLUS" , $startpos} 
  | MINUS { "MINUS" , $startpos } 
  | TIMES { "TIMES" , $startpos} 
  | DIVIDE { "DIVIDE" , $startpos} 
  | EQ { "EQ" , $startpos} 
  | NEQ { "NEQ" , $startpos} 
  | LT { "LT" , $startpos} 
  | LE { "LE" , $startpos} 
  | GT { "GT" , $startpos} 
  | GE { "GE" , $startpos} 
  | AND { "AND" , $startpos} 
  | OR { "OR" , $startpos} 
  | ASSIGN { "ASSIGN" , $startpos} 
  | ARRAY { "ARRAY" , $startpos} 
  | IF { "IF" , $startpos} 
  | THEN { "THEN" , $startpos} 
  | ELSE { "ELSE", $startpos } 
  | WHILE { "WHILE" , $startpos} 
  | FOR { "FOR" , $startpos} 
  | TO { "TO" , $startpos} 
  | DO { "DO" , $startpos} 
  | LET { "LET" , $startpos} 
  | IN { "IN" , $startpos} 
  | END { "END" , $startpos} 
  | OF { "OF" , $startpos} 
  | BREAK { "BREAK" , $startpos} 
  | NIL { "NIL" , $startpos} 
  | FUNCTION { "FUNCTION" , $startpos} 
  | VAR { "VAR" , $startpos} 
  | TYPE { "TYPE" , $startpos} 
  | CARET { "CARET" , $startpos} 

lexdriver: 
  ls = list (anytoken) EOF { ls }


{
open Core
open Menhir_parser

exception LexError of string

let[@inline] failwith msg = raise (LexError msg)

let[@inline] illegal c =
  failwith (Printf.sprintf "[lexer] unexpected character: '%c" c)

(* For string literals *)
let string_buff = Buffer.create 256
let is_in_string = ref false

}

let whitespace = ' ' | '\t'
let newline = "\r\n" | '\r' | '\n'
let ident_start_char = [
  'A'-'Z'
  'a'-'z'
]
let ident_char = ident_start_char | [ '0'-'9' ]
let ident = ident_start_char ident_char*

let decimal_literal =
  '-'? ['0'-'9'] ['0'-'9' '_']*

let int_literal =
  decimal_literal

rule next_token = parse
  | eof { EOF }
  | whitespace+
    { next_token lexbuf }
  | newline
    { Lexing.new_line lexbuf; next_token lexbuf }
  | '(' { LPAR }
  | ')' { RPAR }
  | '{' { LCURLY }
  | '}' { RCURLY }
  | ';' { SEMI }
  | ':' { COLN }
  | '+' { PLUS }
  | '-' { MINUS }
  | '=' { EQUAL }
  | "let" { LET }
  | "in" { IN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "null" { NULL }
  | "true" { TRUE }
  | "false" { FALSE }

  | "\""
    { Buffer.reset string_buff;
      let string_start = lexbuf.lex_start_p in
      is_in_string := true;
      string lexbuf;
      is_in_string := false;
      lexbuf.lex_start_p <- string_start;
      STRING (Buffer.contents string_buff)
    }

  | int_literal as il
    { try
        INT (int_of_string il)
      with Failure _ ->
        failwith "Integer literal overflow"
    }

  | ident as id { IDENT id }

  | _ as chr { illegal chr }

and comment = parse
  | newline { Lexing.new_line lexbuf; next_token lexbuf }
  | _ { comment lexbuf }

and string = parse
  | "\\\""
    { Buffer.add_char string_buff '"';
      string lexbuf }

  | '"' { () }
  | eof
      { is_in_string := false;
        failwith "Unterminated string" }

  | "\\n"
    { Buffer.add_char string_buff '\n';
      string lexbuf }

  | _ as chr
    { Buffer.add_char string_buff chr;
      string lexbuf }

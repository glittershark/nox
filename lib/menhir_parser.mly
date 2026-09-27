%token EOF LPAR RPAR LCURLY RCURLY LET IN SEMI COLN NULL PLUS MINUS IF THEN ELSE
%token TRUE FALSE EQUAL
%token <string> STRING
%token <string> IDENT
%token <int> INT

%left PLUS MINUS

%start <Types.expr> expr_eof

%{ open! Core %}


%%


literal:
  | NULL { Value.Null }
  | TRUE { Value.Bool true }
  | FALSE { Value.Bool false }
  | s=STRING { Value.String s }
  | i=INT { Value.Integer i }
  ;

expr_eof:
  | e=expr; EOF { e }
  ;

expr: e=expr_function { e } ;

expr_function:
  | a=ident COLN body=expr_function
    { { desc = Lam { args = Iarray.singleton a; body } } |> Ast.uncurry_lambda }
  | LET bindings=binding+ IN body=expr_function
    { { desc = Let { bindings = Iarray.of_list bindings; body } } }
  | e=expr_if { e }
  ;

expr_if:
  | IF cond = expr THEN then_ = expr ELSE else_ = expr
    { { desc = If { cond; then_; else_ } } }
  | e=expr_op { e }
  ;

expr_op:
  | lhs=expr_op PLUS rhs=expr_op
    { { Ast.desc = Bin_op { lhs; op = Plus; rhs } } }
  | lhs=expr_op MINUS rhs=expr_op
    { { Ast.desc = Bin_op { lhs; op = Minus; rhs } } }
  | e=expr_app { e }
  ;

expr_app:
  | func=expr_simple args=expr_simple+
    { { Ast.desc = Apply { func; args = Iarray.of_list args } } }
  | e=expr_simple { e }

expr_simple:
  | i=ident { { Ast.desc = Ident i } }
  | l=literal { { Ast.desc = Lit l } }
  | LPAR e=expr RPAR { e }
  | LCURLY bindings=binding* RCURLY
    { { Ast.desc = Attrset (Iarray.of_list bindings) } }
  ;

(* As in nix, a binding name is either a bare identifier or a quoted string, so that
   attributes whose names aren't identifiers can still be written down. *)
attr_name:
  | i=ident { i }
  | s=STRING { Ident.of_string s }
  ;

binding:
  name=attr_name (* TODO: attrpath *)
  EQUAL
  value=expr
  SEMI
  { (~name, ~value) }

ident: i=IDENT { Ident.of_string i }

%%

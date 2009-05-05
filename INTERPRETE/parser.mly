  /* Analyseur lexical pour le cours de programmation fonctionnelle SILR 4 Polytech'Nantes. */
  /* Certaines lignes reprises du code source de OCaml par X. Leroy */

%{ (* prelude en OCaml *)

  open Expr ;;

  let rec make_app e l = match l with
    | a :: [] -> App(e,a)
    | a :: r -> make_app (App (e,a)) r
    (* On a rajoute la ligne suivante pour enlever le warning de compil *)
    | [] -> failwith "Liste vide"
 ;;

 %}
  

/*déclaration des tokens */

%token <int> INT
%token <string> IDENT
%token <Expr.binop > BINOP
  
%token IF THEN ELSE FI
%token LET REC BE IN 
%token LPAREN RPAREN
%token FUN ARROW
%token EOF
%token BINOP
  

/* gestion des priorités/associativités */
%left BINOP
%nonassoc IN
%nonassoc ARROW

  
%start main               /*  entry point    */
%type <Expr.expr> main
%%
  main:
  expr EOF                              { $1                        }
  ;
  
  expr: 
  | simpleexpr                          { $1                        }
  | simpleexpr simpleexprlist           { make_app $1 (List.rev $2) } /* application associative a gauche */
/*  | expr BINOP    expr                  { Binop($2,$1,$3)           } */ /* attention, pas de différence entre les opérateurs */
/*  | LET IDENT BE expr IN expr           { App (Fun($2,$6), $4)      } */
/*  | LET REC IDENT BE expr IN expr       { LetRecIn($3,$5,$7)        } */
  | FUN IDENT ARROW expr                { Fun ($2,$4)               } 
  ;
  
  simpleexpr:
  | LPAREN expr RPAREN                  { $2                        }
  | INT                                 { Num $1                    }
  | IDENT                               { Var $1                    }
  | IF expr THEN expr ELSE expr FI      { Cond ($2, $4, $6)         }
  ;
  
  simpleexprlist:
  | simpleexpr { [$1] }
  | simpleexprlist simpleexpr  { $2 :: $1 }
    /* cette règle permet d'avoir une associativite a gauche */

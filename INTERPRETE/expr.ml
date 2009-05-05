(* vim: ts=4 sw=4 expandtab:
*)
 type binop = Plus | Minus | Equals


type expr = 
    Var of string
  | Num of int
  | Bool of bool
  | App of expr * expr  

  | Fun of string * expr
  | Cond of expr * expr * expr

  | Binop of expr*expr*binop
 (* | Add of expr * expr
  | Egal of expr * expr
  | Not of expr*)

;;


let rec tostring e = match e with
  | Var s -> s
  | Num i -> string_of_int i
  | Bool true -> "true"
  | Bool false -> "false"
  | App (e1, e2) -> "(" ^ tostring e1 ^ " " ^ tostring e2 ^ ")"

  (* Pour fun on a pas besoin de tostring le premier parametre,
     c'est deja une string *)
  | Fun (e1, e2) -> "(" ^ e1 ^ " " ^ tostring e2 ^ ")"
  (* Le cond est du type expr*expr*expr *)
  | Cond (e1, e2, e3) -> "(" ^ tostring e1 ^ " " ^ tostring e2 ^ " " ^ tostring e3 ^ ")"

  | Binop (e1, e2, bop) -> (match(bop) with
        | Plus -> "(" ^ tostring e1 ^ " + " ^ tostring e2 ^ ")"
        | Minus -> "(" ^ tostring e1 ^ " - " ^ tostring e2 ^ ")"
        | Equals -> "(" ^ tostring e1 ^ " == " ^ tostring e2 ^ ")"
        )
 (* | Add (e1, e2) -> "(" ^ tostring e1 ^ " " ^ tostring e2 ^ ")"
  | Egal (e1, e2) -> "(" ^ tostring e1 ^ " " ^ tostring e2 ^")"
  | Not (e1) -> "(" ^ tostring e1 ^")"*)
  ;;
 

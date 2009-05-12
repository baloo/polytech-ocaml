(* vim: ts=4 sw=4 expandtab:
*)
 type binop = Plus | Minus | Equals | Times | Inf | Div | Exp | Sup | And | Or


type expr = 
    Var of string
  | Num of int
  | Bool of bool
  | App of expr * expr  

  | Fun of string * expr
  | Cond of expr * expr * expr

  | Binop of binop*expr*expr
  | Not of expr

  | CondMult of (expr*expr) list*expr

  | LetRecIn of string*expr*expr

;;



(* PrÃ©dicat is_prog indiquant si une expression est un programme ou non*)
let rec is_prog el liste = match el with
  | Var f -> if List.mem f liste then true else false
  | Num f -> true
  | Bool _ -> true
  | Not f -> is_prog f liste
  | App (e1, e2) -> is_prog e1 liste && is_prog e2 liste
  | Fun (e1,e2) -> is_prog e2 liste
  | Cond (e1,e2,e3) -> is_prog e1 liste && is_prog e2 liste && is_prog e3 liste
  | Binop (op, e1, e2) -> is_prog e1 liste && is_prog e2 liste
  | LetRecIn (e1, e2, e3) -> let liste2 = e1::liste in is_prog e2 liste2 && is_prog e3 liste2
  | CondMult (e1, e2) -> List.fold_right (fun x -> fun y -> match x with
                                           | (b1, b2) -> is_prog b1 liste && 
                                                         is_prog b2 liste && 
                                                         y) e1 true && 
                         is_prog e2 liste
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

  | Binop (bop, e1, e2) -> (match(bop) with
        | Plus -> "(" ^ tostring e1 ^ " + " ^ tostring e2 ^ ")"
        | Minus -> "(" ^ tostring e1 ^ " - " ^ tostring e2 ^ ")"
        | Equals -> "(" ^ tostring e1 ^ " == " ^ tostring e2 ^ ")"
        | Times -> "(" ^ tostring e1 ^ " * " ^ tostring e2 ^ ")"
        | Inf -> "(" ^ tostring e1 ^ " <= " ^ tostring e2 ^ ")"
        | Div -> "(" ^ tostring e1 ^ " / " ^ tostring e2 ^ ")"
        | Exp -> "(" ^ tostring e1 ^ " ^ " ^ tostring e2 ^ ")"
        | Sup -> "(" ^ tostring e1 ^ " >= " ^ tostring e2 ^ ")"
        | And -> "(" ^ tostring e1 ^ " && " ^ tostring e2 ^ ")"
        | Or -> "(" ^ tostring e1 ^ " || " ^ tostring e2 ^ ")"
        )
  (* Le Not est du type expr *)
  | Not (e1) -> "(" ^ tostring e1 ^")"

  | LetRecIn(e1, e2, e3) -> "let rec " ^ e1 ^ " = " ^ tostring e2 ^ " in " ^ tostring e3 

  | CondMult(rulelist, default) -> "cond " ^ List.fold_right (fun x -> fun y -> match x with
                                                              | (e1,e2) -> y ^ "| " ^ tostring e1 ^ " -> " ^ tostring e2 ^ "\n")
                                             rulelist ("\n _ -> " ^ tostring default ^ " )")
  
  ;;
 

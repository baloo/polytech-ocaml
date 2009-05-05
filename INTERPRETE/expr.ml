
 type binop = Plus | Minus 


type expr = 
    Var of string
  | Num of int
  | Bool of bool
  | App of expr * expr   

;;


let rec tostring e = match e with
  | Var s -> s
  | Num i -> string_of_int i
  | Bool true -> "true"
  | Bool false -> "false"
  | App (e1, e2) -> "(" ^ tostring e1 ^ " " ^ tostring e2 ^ ")"
;;
 

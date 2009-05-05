(* vim: ts=4 sw=4 expandtab: 
*)

open Expr ;;

let rec subst x v e = match e with

  | Var y -> if x = y then v else (Var y)
  | Num f -> Num f
  | Bool b -> Bool b
  | App (e1, e2) -> App( subst x v e1, subst x v e2)   

(* Rajout du predicat Fun string*expr *)
  | Fun (e1, e2) -> Fun( e1, subst x v e2)
(* Rajout du predicat Cond expr*expr*expr *)
  | Cond (e1, e2, e3) -> Cond( subst x v e1, subst x v e2, subst x v e3)
;;

let rec eval ex = match ex with 
  
  | Num f -> Num f

  | Var v ->  failwith "Variable non liée."

  | App (e1,e2) -> failwith "pas encore implemente"

  | Bool b -> Bool b

(* Rajout du predicat Fun string*expr *)
  | Fun (e1, e2) -> Fun( e1, e2)

(* Rajout du predicat Cond expr*expr*expr *)
  | Cond (expif, expthen, expelse) -> (
      match (eval expif) with
        | Bool true -> eval expthen
        | Bool false -> eval expelse
        | _ -> failwith "not a boolean"
      )

;;


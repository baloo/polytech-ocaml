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
(* Rajout de l'operateur binaire Binop expr*expr*binop *)
  | Binop (bop, e1, e2) -> Binop(bop, subst x v e1, subst x v e2)
  | Not (e1) -> Not(subst x v e1)
  
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

(* Rajout de l'operateur binaire Binop expr*expr*binop *)
  | Binop (bop, e1, e2) -> (
      match (bop) with
        | Plus -> (match(eval e1, eval e2) with
            | (Num f1, Num f2) -> Num (f1+f2)
            | _ -> failwith "erreur de type"
            )
        | Minus -> (match(eval e1, eval e2) with
            | (Num f1, Num f2) -> Num (f1-f2)
            | _ -> failwith "erreur de type"
            )
        | Mult -> (match(eval e1, eval e2) with
            | (Num f1, Num f2) -> Num (f1-f2)
            | _ -> failwith "erreur de type"
            )
        | Equals -> (match(eval e1, eval e2) with
            | (Num n1, Num n2)  -> if n1 = n2
                then Bool true
                else Bool false
            | (Bool b1, Bool b2) -> if b1 = b2
                then Bool true
                else Bool false
            | _ -> failwith "erreur de type"
            )
      )
  | Not (e1) -> (
        match(eval e1) with
            | Bool true -> Bool false
            | Bool false -> Bool true
            | _ -> failwith "erreur de type"
        )
;;


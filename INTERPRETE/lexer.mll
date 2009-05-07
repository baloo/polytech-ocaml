 (* vim: ts=4 sw=4 expandtab:
 *)
 
 {
    open Parser        (* The type token is defined in parser.mli *)
    exception Eof
      
 }
  rule token = parse
      [' ' '\t']     { token lexbuf }     (* skip blanks *)
    | ['\n' ]        { token lexbuf }
    | ['0'-'9']+ as lxm { INT(int_of_string lxm) }
    | ['a'-'z']+ as lxm { match lxm with 
                | "if"   -> IF 
                | "then" -> THEN
                | "else" -> ELSE
                | "fi"   -> FI
                | "let"  -> LET
                | "rec"  -> REC 
                | "in"   -> IN
                | "fun" -> FUN
                
                | _ -> IDENT(lxm) }
    | '+'            { BINOP (Expr.Plus) } 
    | "->"           { ARROW }
    | '-'            { BINOP(Expr.Minus) }
    | '*'            { BINOP(Expr.Times) }
    | '/'            { BINOP(Expr.Div)   }
    | '('            { LPAREN }
    | ')'            { RPAREN }
    | "<="           { BINOP(Expr.Inf)    }
    | "=="           { BINOP(Expr.Equals)}
    | '='            { BE }
    | eof            { EOF }
    
    


let _ =
  let lexbuf = Lexing.from_channel stdin in
  let p = Parser.main Lexer.token lexbuf
  in print_endline (Expr.tostring (Eval.eval p)) ;; 


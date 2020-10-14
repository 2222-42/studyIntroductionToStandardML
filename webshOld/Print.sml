(* SML source file. Copyright (c) by 2222-42 2020.
* 
*)
use "./Types.sml";
use "./chapter16/chap16_3_q9.sml";
signature PRINT =
sig
  val exprToString : Types.expr -> string
  val statementToString : Types.statement -> string
end

structure Print : PRINT =
struct
  local
    structure F = Format
    open Types
  in
    fun exprToString e =
      case e of
         IDEXP s => s
       | FOLLOWEXP (expr, i) => F.format "follow(%s, %d)" [F.S(exprToString expr), F.I i]
       | LINKEXP expr => F.format "link %s" [F.S(exprToString expr)]
       | STREXP s => s

    fun statementToString a = 
      case a of
         VAL(id, expr) => F.format "val %s = %s" [F.S id, F.S(exprToString expr)]
       | EXPR e => exprToString e
       | CD expr => F.format "cd %s" [F.S (exprToString expr)]
       | COPY (expr1, expr2) => F.format "copy %s to %s" [F.S (exprToString expr1), F.S (exprToString expr2)]
       | PRINT expr => F.format "print %s" [F.S (exprToString expr)]
       | USE s => F.format "use %s" [F.S s]
       | HELP => F.format "help" []
       | ENV => F.format "env" []
  end
end

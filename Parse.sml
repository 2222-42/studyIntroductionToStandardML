
use "./Control.sml";
use "./Types.sml";
use "./Lex.sml";
structure Parse =
struct
  local
    open Types Control
    structure L = Lex
  in
    fun parse source =
      let
        fun skip () = (* ; まで読み飛ばす*)
        fun syntaxError () = (* report Error *)
        fun check tk = (* check whether tk or not *)
        fun getInt () = (* read data whose type is int *)
        fun parseExpr () = 
          case L.lex source of
             L.ID(S) =>
                (case s of
                   "link" => (* read link formula *)
                 | "follow" => (* read follow formula*)
                 | _ => IDEXP s)
           | L.STRING s => STREXP s
           | _ => syntaxError()
      in
        case L.nextToken source of
           L.SEMICOLON => (L.lex source;
                           Control.doFirstLinePrompt := true;
                           parse source)
         | L.ID("val") => (* read val sentence *)
         | L.ID("cd") => (* read cd sentence *)
         | L.ID("use") => (* read use sentence *)
         | L.ID("print") => (* read print sentence *)
         | L.ID("copy") => (* read copy sentence *)
         | L.ID("help") => (* read help sentence *)
         | L.ID("env") => (* read env sentence *)
         | _ => EXPR (parseExpr () before check L.SEMICOLON)
      end
  end
end
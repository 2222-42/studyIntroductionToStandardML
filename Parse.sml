
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
          case L.nextToken source of
            L.SEMICOLON => ()
          | _ => (L.initToken(source); skip())

        fun syntaxError () = (* report Error *)
          (skip(); raise Syntax)

        fun check tk = (* check whether tk or not *)
          if L.nextToken(source) = tk
          then ()
          else (skip();raise Syntax)

        fun getInt () = (* read data whose type is int *)
           case L.nextToken(source) of
               L.DIGITS s => valOf(Int.fromString(s))
             | _ => raise Syntax

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
         | L.ID("val") => let
                            val _ = L.lex source
                            val id = case L.lex source of
                                        L.ID s => s
                                      | _ => syntaxError()
                            val _ = check L.EQUALSYM
                            val e = parseExpr ()
                            val _ = check L.SEMICOLON
                          in
                            VAL(id,e)
                          end
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
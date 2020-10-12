signature EVAL =
sig
  type env
  val eval : Types.url -> env -> Types.expr -> Types.value
end

structure Eval : EVAL =
struct
local
  open Types Control
in
  type env = (string list * (string * Types.value) tree) ref
  fun eval root env e = 
    case e of
       STREXP s => URL (Url.parseUrl root s)
     | IDEXP s => (Env.lookUp s env
                    handle NotFound => 
                      raise Runtime "Variable not declared.")
     | LINKEXP e => 
        let
          val v = eval root env e
          val root = (case v of
             URL u => u
           | _ => raise Runtime "Not a url data.")
        in
          ParseHtml.parseHtml (Url.baseUrl root) root
        end
     | FOLLOWEXP (e,i) => 
        let
          val v = eval root env e
        in
          case v of
             PAGE{url, links} => URL(List.nth (links, i-1))
           | _ => raise Runtime "Not a link data"
        end
end
end

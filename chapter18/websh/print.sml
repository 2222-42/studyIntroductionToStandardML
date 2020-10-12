signature PRINT =
sig
  val exprToString : Types.expr -> string
  val statementToString : Types.statement -> string
  val valueToString: Types.value -> string
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
    fun urlToString url =
      let
        fun concatStrList sList = 
          foldr (fn (x, R) => "/"^x^R) "" sList
      in
        case url of
          HTTP {host=host, path=path, anchor=anchor} => 
            let
              val pathValue = (if isSome path then valOf path else [])
              val anchorValue = (if isSome anchor then valOf anchor else "")
            in
              F.format 
                "HTTP {host: %s, path: %s, anchor: %s}"
                [F.S (concatStrList host), F.S (concatStrList pathValue), F.S(anchorValue)]
            end
        | FILE {path=path, anchor=anchor} => 
            let
              val anchorValue = (if isSome anchor then valOf anchor else "")
            in
              F.format 
                "FILE {path: %s, anchor: %s}"
                [F.S (concatStrList path), F.S (anchorValue)]
            end
        | RELATIVE {path=path, anchor=anchor, root=root} => 
            let
              val anchorValue = (if isSome anchor then valOf anchor else "")
            in
              F.format 
                "RELATIVE {path: %s, anchor: %s, root: %s}"
                [F.S (concatStrList path), F.S (anchorValue), F.S (urlToString root)]
            end
      end
    fun valueToString v =
      case v of
         URL u => urlToString u
       | PAGE {url=url, links=urlList} => 
          let
            fun makeIntList (0) = nil
              | makeIntList (n) = makeIntList(n-1) @ [n]
            fun makeFormattedStrList url links =
              let
                val intList = makeIntList(length links)
                fun generateListStr [] [] = ""
                  | generateListStr intList links = (F.format "%3d. %s\n" [F.I (hd intList), F.S (urlToString(hd links))])
                                                    ^ (generateListStr (tl intList) (tl links))
              in
                (F.format "page <%s>\n" [F.S (urlToString url)]) ^ generateListStr intList links
                
              end
          in
            makeFormattedStrList url urlList
          end
  end
end

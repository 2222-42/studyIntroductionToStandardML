structure Websh =
struct
  local
    open Types Control
  in
    fun currentPath() = Url.parseUrl (FILE{path=[], anchor=NONE}) ("file:/"^OS.FileSys.getDir())
    val root = ref (currentPath())
    fun topLoop source env = 
      let
        val s = (Control.doFirstLinePrompt := true;
                 Parse.parse source)
      in
        (case s of
           Types.EXPR e  =>  (* 式の評価 *)
            (* print (Print.statementToString s ^ ";\n") *)
            let val v = Eval.eval (!root) env e
            in (Env.bind("it", v, env);
                Format.printf "val it = %s\n"
                  [Format.S (Print.valueToString v)])
            end
         | VAL (id, e) =>  (* 変数の束縛 *)
            let
              val v = Eval.eval (!root) env e
            in
              (Env.bind(id, v, env);
               Format.printf "val %s = %s\n"
               [Format.S(id), Format.S(Print.valueToString v)])
            end
         | COPY (e, e') => (* URLのコピー*)
            let
              val fromadrs = 
                case Eval.eval(!root) env e of
                   URL(x) => x
                 | _ => raise Runtime "A url expected."
              val toadrs = 
                case Eval.eval(!root) env e' of
                   URL(x) => x
                 | _ => raise Runtime "A url expected."
            in
              CopyUrl.copy fromadrs toadrs
            end
         | CD e => (* ディレクトリの変更 *)
            let
              val v = Eval.eval (!root) env e
            in
              case v of
                 URL (x) => 
                  let
                    val d = (Url.canonicalUrl x)
                  in
                    case d of
                       FILE _ => (root := d;
                                  Format.printf "current dir : %s \n"
                                  [Format.S(Print.valueToString (URL d))])
                     | _ => print "Not a valid directory.\n"
                  end
               (* | pat2 => body2 *)
            end
         | PRINT e => print (Print.statementToString s ^ ";\n") (* ページの印字 *)
         | USE f => (* ファイルの実行 *)
            let
              val s = ExternalIO.openIn (Url.canonicalUrl (Url.parseUrl (!root) f))
              val source' = {stream=s, promptMode=false}
            in
              (topLoop source' env
                handle endOfInput =>
                  (ExternalIO.closeIn s; topLoop source env))
            end
         | HELP => print (Print.statementToString s ^ ";\n") (* ヘルプメッセージの印字 *)
         | ENV => print (Print.statementToString s ^ ";\n") (* 現在の環境の印字 *)
         ;
        topLoop source env) 
      end
        handle Runtime s => (print (s^"\n"); topLoop source env)
        handle Syntax => (print "Syntax error.\n"; topLoop source env)
        handle NotFound => (print "NotFound")
        handle StringSyntax => print("StringSyntax")
        handle urlFormat => print("urlFormat")
    val defaultSource = {stream=ExternalIO.StreamIn(TextIO.stdIn), promptMode=true}
    fun websh () = 
      (Lex.initToken defaultSource;
       Lex.printFirstLine defaultSource;
       topLoop defaultSource (Env.emptyEnv())
       handle endOfInput => (print ("endOfInput"); ())
      )
  end
end


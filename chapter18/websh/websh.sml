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
         | VAL (id, e) => print (Print.statementToString s ^ ";\n") (* 変数の束縛 *)
         | COPY (e, e') => print (Print.statementToString s ^ ";\n") (* URLのコピー*)
         | CD e => print (Print.statementToString s ^ ";\n") (* ディレクトリの変更 *)
         | PRINT e => print (Print.statementToString s ^ ";\n") (* ページの印字 *)
         | USE f => print (Print.statementToString s ^ ";\n") (* ファイルの実行 *)
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


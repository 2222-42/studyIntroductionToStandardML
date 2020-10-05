use "./chapter16/chap16_3_q9.sml";
use "./Lex.sml";

structure Control =
struct
  exception NotFound            (* 名前の検索の失敗 *)
  exception Runtime of string   (* 式評価の失敗 *)
  exception StringSyntax        (* 文字列が閉じていない *)
  exception Syntax              (* 構文エラー *)
  exception endOfInput         (* 入力終了 *)
  exception urlFormat           (* URLフォーマットエラー *)
  val firstLinePrompt = ref "->"
  val secondLinePrompt = ref ">>"
  val doFirstLinePrompt = ref true
end

structure Types = struct 
  datatype expr = 
      STREXP of string 
    | IDEXP of string
    | LINKEXP of expr
    | FOLLOWEXP of expr * int
  datatype statement = 
      VAL of string * expr
    | EXPR of expr
    | CD of expr
    | COPY of expr * expr
    | PRINT of expr
    | USE of string
    | HELP
    | ENV
  datatype url = 
      HTTP of {host: string list, path: string list option, anchor: string option}
    | FILE of {path: string list, anchor: string option}
    | RELATIVE of {path: string list, anchor: string option, root: url}
  datatype value =
      URL of url
    | PAGE of {url: url, links: url list}
end

fun isUpper c = #"A" <= c andalso c <= #"Z"

fun toLower c =
    if isUpper c
    then chr (ord #"a" + (ord c - ord #"A"))
    else c;
fun lower s = implode (map toLower (explode s))

structure Url = struct
  local
    open Types Control
  in
    structure SS = Substring
    exception urlFormat
    fun neq c x = not (x = c)
    fun eq c x = c = x
    fun parseHttp s = 
      let
        val s = if SS.isPrefix "://"  s then
                  SS.triml 3 s
                else raise urlFormat
        val (host, body) = SS.splitl (neq #"/") s
        val domain = map SS.string (SS.tokens (eq #".") host)
        val (path, anchor) =
          if SS.isEmpty body then (NONE, NONE)
          else 
            let val (p, a) = SS.splitl(neq #"#") body
            in (SOME (map SS.string (SS.tokens (eq #"/") p)),
                if SS.isEmpty a then NONE
                else SOME(SS.string(SS.triml 1 a)))
            end
      in {host=domain, path=path, anchor=anchor}
      end
    fun parseFile s = 
      let 
        val s = if SS.isPrefix ":/"  s then
            SS.triml 2 s
          else raise urlFormat
        val (path, anchor) =
          if SS.isEmpty s then ([""], NONE)
          else 
            let val (p, a) = SS.splitl(neq #"#") s
            in ((map SS.string (SS.tokens (eq #"/") p)),
                if SS.isEmpty a then NONE
                else SOME(SS.string(SS.triml 1 a)))
            end
      in {path=path, anchor=anchor}
      end
    fun parseRelative s root = 
      let 
        val (path, anchor) =
          if SS.isEmpty s then ([""], NONE)
          else 
            let val (p, a) = SS.splitl(neq #"#") s
            in ((map SS.string (SS.tokens (eq #"/") p)),
                if SS.isEmpty a then NONE
                else SOME(SS.string(SS.triml 1 a)))
            end
      in {path=path, anchor=anchor, root=root}
      end
    fun parseUrl (s, root) = 
      let 
        val s = SS.full s 
        val (scheme, body) = SS.splitl(fn c => c <> #":") s
      in
        if SS.isEmpty body then
          RELATIVE (parseRelative scheme (FILE{path=[], anchor=NONE}))
        else
          case lower (SS.string scheme) of
            "http" => HTTP (parseHttp body)
          | "file" => FILE (parseFile body)
          | _ => raise urlFormat
      end
  end
end

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
          else (skip();
                (* 以下の一行はテスト用のため *)
                print ("token is different: " ^ L.toString(L.nextToken(source)));
                raise Syntax)

        fun getInt () = (* read data whose type is int *)
           case L.nextToken(source) of
               L.DIGITS s => valOf(Int.fromString(s))
             | _ => raise Syntax

        fun parseExpr () = 
          (* 以下のlet in endでくくってテストに表示させるようにする *)
          let
            val exp = L.lex source
          in
            (* print (L.toString exp ^ "\n"); *)
            case exp of
              L.ID(s) =>
                  (case s of
                    "link" => (* read link formula *)
                        let
                          val _ = L.lex source
                          val _ = check L.LPAREN
                          val e = parseExpr ()
                          val _ = check L.RPAREN
                        in
                          LINKEXP e
                        end
                  | "follow" => (* read follow formula*)
                        let
                          val _ = L.lex source
                          val _ = check L.LPAREN
                          val e = parseExpr ()
                          val _ = check L.COMMA
                          val i = case L.lex source of
                                      L.DIGITS n => valOf(Int.fromString n)
                                    | _ => syntaxError()
                          val _ = check L.RPAREN
                        in
                          FOLLOWEXP(e, i)
                        end
                  | _ => IDEXP s)
            | L.STRING s => STREXP s
            | L.EOF => raise endOfInput (* C-dでParseErrorが発生することへの対処 *)
            | _ => (print "parse Error\n";
                    syntaxError())
        end
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
                            val _ = L.lex source
                            val e = parseExpr ()
                            val _ = check L.SEMICOLON
                          in
                            VAL(id,e)
                          end
         | L.ID("cd") => (* read cd sentence *)
                          let
                            val _ = L.lex source
                            val e = parseExpr ()
                            val _ = check L.SEMICOLON
                          in
                            CD e
                          end
         | L.ID("use") => (* read use sentence *)
                          let
                            val _ = L.lex source
                            val id = case L.lex source of
                                        L.ID s => s
                                      | _ => syntaxError()
                            val _ = check L.SEMICOLON
                          in
                            USE id
                          end
         | L.ID("print") => (* read print sentence *)
                          let
                            val _ = L.lex source
                            val e = parseExpr ()
                            val _ = check L.SEMICOLON
                          in
                            PRINT e
                          end
         | L.ID("copy") => (* read copy sentence *)
                          let
                            val _ = L.lex source
                            val e1 = parseExpr ()
                            val _ = check (L.ID "to")
                            val _ = L.lex source
                            val e2 = parseExpr ()
                            val _ = check L.SEMICOLON
                          in
                            COPY (e1,e2)
                          end
         | L.ID("help") => (* read help sentence *)
                          let
                            val _ = L.lex source
                            val _ = check L.SEMICOLON
                          in
                            HELP
                          end
         | L.ID("env") => (* read env sentence *)
                          let
                            val _ = L.lex source
                            val _ = check L.SEMICOLON
                          in
                            ENV
                          end
         | _ => EXPR (parseExpr () before check L.SEMICOLON)
      end
  end
end

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

signature EVAL =
sig
  type env
  val eval : Types.url -> env -> Types.expr -> Types.value
end

signature ENV =
sig
  type env
  val emptyEnv : unit -> env
  val bind : string * Types.value * env -> unit
  val lookUp : string -> env -> Types.value
  val domain : env -> string list
end

datatype 'a tree = Empty | Node of 'a * 'a tree * 'a tree;

structure Env : ENV = 
struct
  type env = (string list * (string * Types.value) tree) ref
  (* 'a を仮に string とする -> Types.value *)
  fun emptyEnv () = (ref (nil, Empty) : env)
  fun enter (key:string, v, env) = 
    case env of 
        (keyList, Empty) => 
              (keyList @ [key], Node((key, v), Empty, Empty))
        | (keyList, Node((key', v'), L, R)) =>
            if key = key' then 
              (keyList, Node((key', v'), L, R))
            else if key > key' then 
            (*   operator domain: 'Z ref * 'Z
                 operand:         (string * 'Y) tree * 'X *)
              (* enter (key, v, R) *)
              let
                val (newKeyList, newTree) = enter (key, v, (keyList, R))
              in
                (newKeyList, Node((key', v'), L, newTree))
              end
            else 
            (* enter (key, v, L) *)
              let
                val (newKeyList, newTree) = enter (key, v, (keyList, L))
              in
                (newKeyList, Node((key', v'), newTree, R))
              end
            
  (* fun addKey (key, list) =
    let val newList = foldr (fn (h, R) => if (h = key) then R else R@[h]) [] list
    in 
      list := newList
    end *)

(*   spec:   string * Types.value * ?.Env.env -> unit
  actual: string * 'a * (string list * 'b) ref -> string list * _ *)
  (* fun bind (key: string, v:Types.value, ref (keyList, dict): env) = 
      (addKey (key, keyList); enter(key, v, env); ()) *)
  fun bind (key: string, v:Types.value, env) = 
    let
      val newTree = enter(key, v, !env)
    in
      env := newTree
    end
    (* (enter(key, v, env); ()) *)

(*    expression:  Types.value env -> Types.value
  result type:  (string * Types.value) tree -> Types.value *)
  (* fun lookUp (key:string) (ref (_, Empty): Types.value env) = raise Control.NotFound
    | lookUp key (ref (_, Node((key', v), L, R)): Types.value env) =
        if key = key' then v
        else if key > key' then lookUp key R
        else lookUp key L *)
  
  (* val lookUp : string -> env -> Types.value *)
  fun lookUpSub (key:string) env = 
    case env of
       (_, Empty) => raise Control.NotFound
     | (keyList, Node((key', v), L, R)) => 
        if key = key' then v
        else if key > key' then lookUpSub key (keyList,R)
        else lookUpSub key (keyList,L)
  
  fun lookup (key: string) (env:env) =
    lookUpSub key (!env)

  fun domain env =
    case !env of
      ([], _) => []
    | (list, _) => list

end

structure Websh =
struct
  local
    open Types Control
  in
    fun currentPath() = Url.parseUrl("file:/"^OS.FileSys.getDir(), OS.FileSys.getDir())
    val root = ref (currentPath())
    fun topLoop source env = 
      let
        val s = (Control.doFirstLinePrompt := true;
                 Parse.parse source)
      in
        (case s of
           EXPR e  => print (Print.statementToString s ^ ";\n") (* 式の評価 *)
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
    val defaultSource = {stream=TextIO.stdIn, promptMode=true}
    fun websh () = 
      (Lex.initToken defaultSource;
       Lex.printFirstLine defaultSource;
       topLoop defaultSource (Env.emptyEnv())
       handle endOfInput => ()
      )
  end
end

(*  

use "./Parse.sml";
- Parse.parse;
val it = fn : Lex.source -> Types.statement

use "./Print.sml";

- Parse.parse;
val it = fn : Lex.source -> ?.Types.statement
- Print.statementToString;
val it = fn : Types.statement -> string

型変数が導入されてしまう。同じ型なので？
 *)

(* 
Websh.websh ();
"a";

copy 
it to 
"a"'

修正すること
1. 読み込み時に
`->`
が表示されないこと

2. C-dでParseErrorが発生すること

*)
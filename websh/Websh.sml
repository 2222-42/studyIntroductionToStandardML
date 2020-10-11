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

signature URL = sig
  val parseUrl : Types.url -> string -> Types.value
  val urlToString : Types.url -> string
  val canonicalUrl : Types.url -> Types.url
  val baseUrl : Types.url -> Types.url
  val nodeUrl : Types.url -> string option
  (* val pathUrl : Types.url -> string list *)
  val joinUrlFile : Types.url -> string -> Types.url
  val joinUrlPath : Types.url -> string list -> Types.url
  (*  val isRelative: Types.url -> bool *)
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
          let val (p, a) = SS.splitl (fn #"#" => false | _ => true) s
          in (map SS.string 
                (SS.fields (fn c => c = #"/") p),
              if SS.isEmpty a then 
                NONE
              else SOME (SS.string (SS.triml 1 a))
          )
          end
      in 
        case path of
           ("" :: L) => 
            (case root of
               HTTP{path,host,...} => HTTP{host=host, path=SOME L, anchor=anchor}
             | FILE _ => FILE{path=L, anchor=anchor})
         | _ => RELATIVE {path=path, anchor=anchor, root=root}
      end
      (* let 
        val (path, anchor) =
          if SS.isEmpty s then ([""], NONE)
          else 
            let val (p, a) = SS.splitl(neq #"#") s
            in ((map SS.string (SS.tokens (eq #"/") p)),
                if SS.isEmpty a then NONE
                else SOME(SS.string(SS.triml 1 a)))
            end
      in {path=path, anchor=anchor, root=root}
      end *)
    fun parseUrl root s = 
      let 
        val s = SS.full s 
        val (scheme, body) = SS.splitl(fn c => c <> #":") s
      in
        if SS.isEmpty body then
          parseRelative scheme root
        else
          case lower (SS.string scheme) of
            "http" => HTTP (parseHttp body)
          | "file" => FILE (parseFile body)
          | _ => parseRelative scheme root
      end
    fun isFileName string =
      let
        fun splitCharsDot chrList = 
          case chrList of
             [] => ([], [])
           | c :: chrList' => 
              if c = #"." then ([], chrList') 
              else let val (l, r) = splitCharsDot chrList'
                   in (c::l, r)
                   end
        val (l, r) = splitCharsDot(String.explode (string))
      in
        not (length r = 0)
      end
    (* TODO: HTTPの場合のpathからの取得や削除の修正 *)
    fun baseUrl url = 
      case url of
         HTTP {host=host, path=path, anchor=anchor} => 
            (* case path of
               NONE =>  *)
                  if length host = 0 then HTTP {host=host, path=path, anchor=anchor}
                  else if isFileName (List.last host) then  HTTP {host=List.take(host, length(host) - 1),path=path, anchor=anchor}
                  else HTTP {host=host, path=path, anchor=anchor}
             (* | SOME v => 
                  if length v = 0 then HTTP {host=host, path=path, anchor=anchor}
                  else if isFileName (List.last v) then  HTTP {host=host,path=SOME(List.take(v, length(v) - 1)), anchor=anchor}
                  else HTTP {host=host, path=path, anchor=anchor} *)
         (* HTTP {host=host, path=path, anchor=anchor} *)
       | FILE {path=path, anchor=anchor} => 
            if length path = 0 then FILE {path=path, anchor=anchor}
            else if isFileName (List.last path) then  FILE {path=List.take(path, length(path) - 1), anchor=anchor}
            else FILE {path=path, anchor=anchor}
       (* FILE {path=path, anchor=anchor} *)
       | RELATIVE {path=path, anchor=anchor, root=root} => 
            if length path = 0 then RELATIVE {path=path, anchor=anchor, root=root}
            else if isFileName (List.last path) then  RELATIVE {path=List.take(path, length(path) - 1), anchor=anchor, root=root}
            else  RELATIVE {path=path, anchor=anchor, root=root}
          (* RELATIVE {path=path, anchor=anchor, root=root} *)
    fun nodeUrl url = 
      case url of 
         HTTP {host=host, path=path, anchor=anchor} => 
            if length host = 0 then ""
            else if isFileName (List.last host) then List.last host
            else ""
       | FILE {path=path, anchor=anchor} => 
            if length path = 0 then ""
            else if isFileName (List.last path) then List.last path
            else ""
       | RELATIVE {path=path, anchor=anchor, root=root} => 
            if length path = 0 then ""
            else if isFileName (List.last path) then List.last path
            else ""
    fun joinUrlFile url fileName = 
      case url of
         HTTP {host=host, path=path, anchor=anchor} => 
            HTTP {host=host@[fileName], path=path, anchor=anchor}
       | FILE {path=path, anchor=anchor} => 
            FILE {path=path@[fileName], anchor=anchor}
       | RELATIVE {path=path, anchor=anchor, root=root} => 
            RELATIVE {path=path@[fileName], anchor=anchor, root=root}
    fun addToOrRemoveFromStringList sourceList relativeList = 
      foldl (fn (x, R) => if x = ".." then List.take(R, length(R) - 1)
                          else if x = "." then R
                          else R @ [x]) sourceList relativeList
    fun addToOrRemoveFromListOption sourceListOption relativeList = 
      case sourceListOption of
         SOME v => SOME(addToOrRemoveFromStringList v relativeList)
       | NONE => SOME(addToOrRemoveFromStringList [] relativeList)
    fun joinUrlPath url stringList = 
      case url of
         HTTP {host=host, path=path, anchor=anchor} => 
            HTTP {host=host, path=(addToOrRemoveFromListOption path stringList), anchor=anchor}
       | FILE {path=path, anchor=anchor} => 
            FILE {path=(addToOrRemoveFromStringList path stringList), anchor=anchor}
       | RELATIVE {path=path, anchor=anchor, root=root} => 
            RELATIVE {path=(addToOrRemoveFromStringList path stringList), anchor=anchor, root=root}
    fun urlToString url = 
      let 
        fun splice (nil,_) = ""
          | splice ([x],_) = x
          | splice ((h::t),s) = h ^ s ^ splice (t,s);
      in
        case url of 
          HTTP {host=host, path=path, anchor=anchor} => 
            "http://" ^
            (splice (host, "/")) ^
            (case path of
              NONE => ""
            | SOME li => "/"^splice(li, "/")) ^
            (case anchor of
              NONE => ""
            | SOME s => "#" ^ s)
        | FILE {path=path, anchor=anchor} => 
            "file:///" ^
            (splice (path, "/")) ^
            (case anchor of
              NONE => ""
            | SOME s => "#" ^ s)
        | RELATIVE {path=path, anchor=anchor, root=root} =>
            (splice (path, "/")) ^
            (case anchor of
              NONE => ""
            | SOME s => "#" ^ s) ^
            " [on " ^
            (urlToString root) ^ "]"
      end
    fun canonicalUrl url = 
      case url of
         HTTP {host=host, path=path, anchor=anchor} => 
            let val canonicArcString = OS.Path.fromString(
              OS.Path.mkCanonical(
                OS.Path.toString{arcs=host, isAbs=true, vol=""}
              )
            )
            in 
              (fn {arcs,...} => HTTP{host=arcs, path=path, anchor=anchor})canonicArcString
            end
       | FILE {path=path, anchor=anchor} => 
            let val canonicArcString = OS.Path.fromString(
              OS.Path.mkCanonical(
                OS.Path.toString{arcs=path, isAbs=true, vol=""}
              )
            )
            in 
              (fn {arcs,...} => FILE{path=arcs, anchor=anchor})canonicArcString
            end
       | RELATIVE {path=path, anchor=anchor, root=root} => 
            let 
              val canonicRootUrl = canonicalUrl root
              val canonicPathString = OS.Path.fromString(
                OS.Path.mkCanonical(
                  OS.Path.toString{arcs=path, isAbs=true, vol=""}
                )
              )
            in
              case canonicRootUrl of
                 HTTP {host=hostRoot, path=pathRoot, anchor=anchorRoot} => 
                  let
                    val canonicPathString = OS.Path.fromString(
                      OS.Path.mkCanonical(
                        OS.Path.toString{arcs=(hostRoot @ path), isAbs=true, vol=""}
                      )
                    )
                  in
                    (fn {arcs, ...} =>HTTP {host=arcs, path=pathRoot, anchor=anchor}) canonicPathString
                  end
               | FILE {path=pathRoot, anchor=anchorRoot} => 
                  let
                    val canonicPathString = OS.Path.fromString(
                      OS.Path.mkCanonical(
                        OS.Path.toString{arcs=(pathRoot @ path), isAbs=true, vol=""}
                      )
                    )
                  in
                    (fn {arcs, ...} =>FILE {path=arcs, anchor=anchor}) canonicPathString
                  end
            end
       
  end
end

(* 
val testUrl = Url.parseUrl (Types.FILE{path=[], anchor=NONE}) "http://www.jaist.ac.jp/~ohori/test";
Url.urlToString testUrl;
val testFile = Url.parseUrl (Types.FILE{path=[], anchor=NONE}) "file:///mnt/e/SMLProject/";
Url.urlToString testFile;
val testRelative = Url.parseUrl testFile "./studyingStandardML/Url.sml";
(  RELATIVE
    {anchor=NONE,path=[".","studyingStandardML","Url.sml"],
     root=FILE {anchor=NONE,path=[#,#,#]}} : Types.url)
Url.urlToString testRelative;
(val it = "./studyingStandardML/Url.sml [on file:///mnt/e/SMLProject]" : string)
val testRelative2 = Url.parseUrl testFile "/studyingStandardML/Url.sml";
Url.urlToString testRelative2;

val url = testRelative;
val path = (fn Types.RELATIVE{path,...} => path) url;

val url = testUrl;
val path = (fn Types.HTTP{host,...} => host) url;
OS.Path.toString{arcs=path, isAbs=true, vol=""};
OS.Path.mkCanonical it;
OS.Path.fromString it;

Url.canonicalUrl testUrl;
Url.canonicalUrl testFile;
Url.canonicalUrl testRelative;
Url.baseUrl testRelative
Url.canonicalUrl it;

Url.joinUrlPath testUrl ["testDir", "testSubDir", "testFileName"];
Url.joinUrlPath testFile ["testDir", "testSubDir", "testFileName"];
Url.joinUrlPath testRelative ["testDir", "testSubDir", "testFileName"];

Url.joinUrlPath testUrl [".", "testDir", "testSubDir", "testFileName"];
Url.joinUrlPath testFile [".", "testDir", "testSubDir", "testFileName"];
Url.joinUrlPath testRelative [".", "testDir", "testSubDir", "testFileName"];

Url.joinUrlPath testUrl ["..", "testDir", "testSubDir", "testFileName"];
Url.joinUrlPath testFile ["..", "testDir", "testSubDir", "testFileName"];
Url.joinUrlPath testRelative ["..", "testDir", "testSubDir", "testFileName"];


*)

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
(* 
val cp = Websh.currentPath();
val url2 = Types.FILE{path=["test"], anchor=NONE};
val page = Types.PAGE {url=cp, links=[cp, cp, url2]};
Print.valueToString page;
print it;
*)

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
  fun emptyEnv () = (ref (nil, Empty) : env)
  fun enter (key:string, v, env) = 
    case env of 
        (keyList, Empty) => 
              (keyList @ [key], Node((key, v), Empty, Empty))
        | (keyList, Node((key', v'), L, R)) =>
            if key = key' then 
              (keyList, Node((key', v'), L, R))
            else if key > key' then 
              let
                val (newKeyList, newTree) = enter (key, v, (keyList, R))
              in
                (newKeyList, Node((key', v'), L, newTree))
              end
            else 
              let
                val (newKeyList, newTree) = enter (key, v, (keyList, L))
              in
                (newKeyList, Node((key', v'), newTree, R))
              end

  fun bind (key: string, v:Types.value, env) = 
    env := enter(key, v, !env)

  fun lookUp (key: string) (env: env) =
    (* Types.URL (Types.HTTP {host=[], path=NONE, anchor=NONE}) *)
    case env of
       ref (_, Empty) => raise Control.NotFound
     | ref (keyList, Node((key', v), L, R)) => 
        if key = key' then v
        else if key > key' then lookUp key (ref (keyList,R):env)
        else lookUp key (ref (keyList,L):env)

  fun domain env =
    case !env of
      ([], _) => []
    | (list, _) => list

end

signature PARSEHTML =
sig
  val parseHtml : Types.url -> Types.url -> Types.value
end

structure ParseHtml =
struct
local open Types
in
  fun parseHtml url1 url2 =
    PAGE{url=url2, links=nil}
end
end

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
    val defaultSource = {stream=TextIO.stdIn, promptMode=true}
    fun websh () = 
      (Lex.initToken defaultSource;
       Lex.printFirstLine defaultSource;
       topLoop defaultSource (Env.emptyEnv())
       handle endOfInput => (print ("endOfInput"); ())
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
link "test"

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

(* 
Websh.currentPath();
*)
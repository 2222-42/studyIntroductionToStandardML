(* SML source file. Copyright (c) by 2222-42 2020.
* Chap18.2 Chap18.3 Q18.1
*)

(* Chap 18.2 *)
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

structure Control =
struct
  exception NotFound            (* 名前の検索の失敗 *)
  exception Runtime of string   (* 式評価の失敗 *)
  exception StringSyntax        (* 文字列が閉じていない *)
  exception Syntax              (* 構文エラー *)
  exception endOfIntput         (* 入力終了 *)
  exception urlFormat           (* URLフォーマットエラー *)
  val firstLinePrompt = ref "->"
  val secondLinePrompt = ref ">>"
  val doFirstLinePrompt = ref true
end

(* Chap18.4 *)

(* Q18.1 *)

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

(* Chap 18.3 *)
structure Websh =
struct
  local
    open Types Control
  in
    fun currentPath() = Url.parseUrl("file:/"^OS.FileSys.getDir(), OS.FileSys.getDir())
    val root = ref (currentPath())
    (* fun topLoop source env = 
      let
        val s = (Control.doFirstLinePrompt := true;
                 Parse.parse source)
                 (* Parseは未定義 *)
      in
        (case s of
           EXPR e  => Eval(* 式の評価 *)
         | VAL (id, e) => (* 変数の束縛 *)
         | COPY (e, e') => (* URLのコピー*)
         | CD e => (* ディレクトリの変更 *)
         | PRINT e => (* ページの印字 *)
         | USE f => (* ファイルの実行 *)
         | HELP => (* ヘルプメッセージの印字 *)
         | ENV => (* 現在の環境の印字 *)
         ;
        topLoop source env) 
      end
    val defaultSource = {stream=TextIO.stdIn, promptMode=true}
    fun websh () = 
      (Lex.initLex defaultSource;
       topLoop defaultSource (Env.emptyEnv())
       handle endOfInput => ()
      ) *)
  end
end;

Websh.currentPath();
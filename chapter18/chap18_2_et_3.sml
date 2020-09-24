(* SML source file. Copyright (c) by 2222-42 2020.
* Chap18.2 Chap18.3 
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

(* Chap 18.3 *)
structure Websh =
struct
  local
    open Types Control
  in
    
  end
end
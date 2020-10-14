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
(* SML source file. Copyright (c) by 2222-42 2020.
* Chap 8.2
*)

(* 履歴に依存するプログラム *)

(* 
生成した最後の名前の内部表現を保持する参照型データ state
次の名前の内部表現を求める関数 next
内部表現を文字列に変換する関数 toString
*)

(* local
    val state = 
    fun toString =
    fun next s =
in fun gensym() = (state:=next (!state); toString(!state))
end *)
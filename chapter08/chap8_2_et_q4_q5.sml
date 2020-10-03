(* SML source file. Copyright (c) by 2222-42 2020.
* Chap 8.2 Q8.4 Q8.5
*)

(* 履歴に依存するプログラム *)

(* 
生成した最後の名前の内部表現を保持する参照型データ state
次の名前の内部表現を求める関数 next
内部表現を文字列に変換する関数 toString
*)

local
    val state = ref nil : int list ref
    fun toString L = implode (map chr (rev L))
    fun next nil = [ord #"a"]
      | next (h::t) = if h = ord #"z" then
                        ord #"a" :: (next t)
                      else (h+1::t)
in fun gensym() = (state:=next (!state); toString(!state))
end;

fun last [a] = a
  | last (_::t) = last t;

fun drop 0 l = l 
    | drop n l = drop (n - 1) (tl l);

fun take n l = hd (drop n l);

fun makeGensym CharList =
    let
        val state = ref nil : int list ref
        (* val firstChr = hd testCharList
        val lastChr = last CharList *)
        fun toString L = implode (map (fn x => take x CharList) (rev L))
        fun next nil = [0]
          | next (h::t) = if h = (length CharList - 1) then
                              0 :: (next t)
                          else (h+1::t)
    in
        fn () => (state:=next (!state); toString(!state))
    end;
val testCharList = [#"S", #"M", #"L"];

val test = makeGensym testCharList;
test();
test();

(* 筆者の回答 *)
fun makeGenSym L =
    let val seed = ref [0]
        fun next nil = [0]
        | next (h::t) = if h = (length L - 1) then 0::(next t) else (h+1)::t
        fun toString s = implode (map (fn x => List.nth (L,x)) (rev s))
    in
    fn () => toString (!seed) before seed := next (!seed)
    end;

val test = makeGensym testCharList;
test();
test();
(* List.nth を使っているが、これはまだ本では導入されていなかったと思う。 *)
(* 
beforeを使うか;を使うか、
- `exp1 before exp2`: exp1とexp2を評価し、exp1の値を返す
- `(exp1; exp2)`: exp1とexp2を評価し、exp2の値を返す *)
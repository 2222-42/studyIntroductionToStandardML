(* SML source file. Copyright (c) by 2222-42 2020.
* Q 7.11 7.12
*)

(* データ型の使用例として、2分木を使って値の登録と検索を行う辞書プログラム *)
datatype 'a tree = Empty | Node of 'a * 'a tree * 'a tree;

type ('a, 'b) dict = ('a * 'b) tree;

fun makeEnter operation = 
    let 
        fun enter (key, v, dict) =
            case dict of 
            Empty => Node((key, v), Empty, Empty)
            | Node((key', v'), L, R) =>
                if operation(key', key) then 
                    Node((key', v'), L, enter (key, v, R))
                else if operation(key, key') then 
                    Node((key', v'), enter (key, v, L), R)
                else dict
    in
        enter
    end;
(* これだと以下のように、型が想定しているものと異なることになってしまう
expected: ('a * 'a -> order) -> 'a * 'b * ('a,'b) dict -> ('a,'b) dict
result: ('a * 'a -> bool) -> 'a * 'b * ('a * 'b) tree -> ('a * 'b) tree
*)
(* val enter = fn dict => makeEnter (op <) dict;  *)
(* val enter = fn : int * 'a * (int * 'a) tree -> (int * 'a) tree *)

(* 筆者の解答: *)
(* fun makeEnter f (k, v, Empty) = Node((k,v),Empty,Empty)
  | makeEnter f (k, v, dict as Node((k',v'),L,R)) =
    (case f (k,k') of
        EQUAL => dict
      | GREATER => Node((k',v'),L,makeEnter f (k,v,R))
      | LESS => Node((k',v'),makeEnter f (k,v,L),R)); *)
(*
letなどを使わず、単純になっている。 
as使って第三引数のdict形式を指定している。それだけ考えればいいのだから。 
また、datatype orderの
- EQUAL
- GREATER
- LESS
を使うことで内容も分かりやすく、かつ、回答者のどちらかわからない結果ではなくしている。
*)
(* 筆者の回答の改善版 
makeEnterの型は、foldlなどと一緒に使うことを考えると、
   val makeEnter : (’a * ’a -> order) -> (’a * ’b) * (’a,’b) dict -> (’a,’b) dict
の方が便利である。 そこで、この型の関数として定義例を以下に示す
*)
fun makeEnter f ((k, v), Empty) = Node((k,v), Empty, Empty)
    | makeEnter f ((k, v), dict as Node((k',v'),L,R)) =
    (case f (k,k') of
        EQUAL => dict
        | GREATER => Node((k',v'), L, makeEnter f ((k,v), R))
        | LESS => Node((k',v'), makeEnter f ((k,v),L), R))
val enter = fn dict => makeEnter Int.compare dict; 


(* 回答者の回答 *)
fun makeLookUp operation = 
    let 
        fun lookup (key, Empty) = NONE
            | lookup (key, Node((key', v), L, R)) =
                if operation(key', key) then 
                    lookup(key, R)
                else if operation(key, key') then 
                    lookup(key, L)
                else SOME v
    in
        lookup
    end;

(* これだと以下のように、型が想定しているものと異なることになってしまう
expected: ('a * 'a -> order) -> 'a * 'b * ('a,'b) dict -> ('a,'b) dict
result: ('a * 'a -> bool) -> 'a * ('a * 'b) tree -> 'b option
*)

(* 筆者の回答: *)
fun makeLookUp f (k, Empty) = NONE
  | makeLookUp f (k, dict as Node((k', v'), L, R)) =
    (case f (k, k') of
        EQUAL => SOME(v')
      | GREATER => makeLookUp f (k,L)
      | LESS => makeLookUp f (k,R));

(* 筆者の回答：完全版 *)
fun makeLookUp f (k, Empty) = NONE
    | makeLookUp f (k, dict as Node((k',v'),L,R)) =
    (case f (k,k') of
        EQUAL => SOME v'
        | GREATER => makeLookUp f (k, R)
        | LESS => makeLookUp f (k, L))

(* val enter = fn dict => makeEnter  dict;  *)
(* val enter = makeEnter (op <);  *)
(* stdIn:18.2-28.11 Warning: type vars not generalized because of
   value restriction are instantiated to dummy types (X1,X2,...)
val enter = fn : int * ?.X1 * (int * ?.X1) tree -> (int * ?.X1) tree *)
val lookup = fn dict => makeLookUp Int.compare dict;

fun makeDict L =
   foldr
     (fn ((key, v), dict) => enter((key, v), dict))
     Empty
     L;

(* Q7.12 *)

Control.Print.printDepth := 20;
val testStrList = [(1, "a"), (2, "b"), (3,"c"), (21, "ba"), (23, "bc")];
val testDict = makeDict testStrList;

lookup (0, testDict);
val updateDict0 = enter ((0, " "),testDict);
lookup (0, updateDict0);
val updateDict10 = enter ((10, "ca"),testDict);
val updateDict22 = enter ((22, "bb"),testDict);
val updateDict30 = enter ((30, "z"),testDict);


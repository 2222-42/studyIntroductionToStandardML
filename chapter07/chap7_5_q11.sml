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

val enter = fn dict => makeEnter (op <) dict; 
(* val enter = makeEnter (op <);  *)
(* stdIn:18.2-28.11 Warning: type vars not generalized because of
   value restriction are instantiated to dummy types (X1,X2,...)
val enter = fn : int * ?.X1 * (int * ?.X1) tree -> (int * ?.X1) tree *)
val lookup = fn dict => makeLookUp (op <) dict;

fun makeDict kVList =
    case kVList of 
        nil => Empty
        | ((k, value)::t) => enter (k, value, makeDict(t));

Control.Print.printDepth := 20;
val testStrList = [(1, 1), (2, 2), (3, 3), (21, 21), (23, 23)];
val testDict = makeDict testStrList;

lookup (0, testDict);
val updateDict0 = enter (0, 0,testDict);
lookup (0, updateDict0);
val updateDict10 = enter (10, 10,testDict);
val updateDict22 = enter (22, 22,testDict);
val updateDict30 = enter (30, 30,testDict);
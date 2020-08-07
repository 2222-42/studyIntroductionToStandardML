(* SML source file. Copyright (c) by 2222-42 2020.
* Chap 9.2
*)

datatype 'a tree = Empty | Node of 'a * 'a tree * 'a tree;

type 'a dict = (string * 'a) tree;

(* fun enter (key:string, v, dict) = 
    case dict of 
        Empty => Node((key, v), Empty, Empty)
        | Node((key', v'), L, R) =>
            if key = key' then dict
            else if key > key' then 
                Node((key', v'), L, enter (key, v, R))
            else Node((key', v'), enter (key, v, L), R); *)

(* 例外処理を導入すれば検索結果が辞書に存在したか否かのテストが不要になる *)
exception NotFound;

fun lookUp (key:string, Empty) = raise NotFound
    | lookUp (key, Node((key', v), L, R)) =
        if key = key' then v
        else if key > key' then lookUp(key, R)
        else lookUp(key, L);

fun assoc (nil, dict) = nil
  | assoc ((h::t), dict) =
        (h, lookUp(h, dict))::assoc(t, dict)
        handle NotFound => (print "Undefined key. \n"; nil);

(* Q9.2 *)
exception DuplicateEntry;

fun enter (key, v, dict) = 
    case dict of 
        Empty => Node((key, v), Empty, Empty)
        | Node((key', v'), L, R) =>
            if key = key' then raise DuplicateEntry
            else if key > key' then 
                Node((key', v'), L, enter (key, v, R))
            else Node((key', v'), enter (key, v, L), R);

(* 筆者の解答:
   fun enter (key,v,dict) =
       case dict of
            Empty => Node((key,v),Empty,Empty)
          | Node((key',v'),L,R) =>
              if key = key' then raise DuplicateEntry
              else if key > key' then
                   Node((key',v'),L, enter (key,v,R))
              else Node((key',v'),enter (key,v,L),R)
完全一致
*)
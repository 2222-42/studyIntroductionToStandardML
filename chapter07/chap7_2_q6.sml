(* SML source file. Copyright (c) by 2222-42 2020.
* Q7.4 Q 7.6
*)

datatype 'a tree = Empty | Node of 'a * 'a tree * 'a tree;

(* 1 *)

fun countNodes t = 
    case t of Empty => 0
            | Node(_, t1, t2) => 1 + countNodes t1 + countNodes t2;

val testNode = Node (1,Node (2,Empty,Empty),Node (3,Node (4, Empty, Empty),Empty));

countNodes testNode;

(* 筆者の解答:case文を使わない場合 *)
fun nodes Empty = 0
  | nodes (Node(_, L, R)) = 1 + nodes L + nodes R;

(* 2 *)

fun sumTree Empty = 0
  | sumTree (Node (x, t1, t2)) = x + sumTree t1 + sumTree t2;

sumTree testNode;

(* 3 *)

fun mapTree f t =
    case t of Empty => Empty
            | Node(a, t1, t2) => Node(f(a), mapTree f t1, mapTree f t2);

mapTree (fn x => x + 1) testNode;

(* 筆者の解答: case文を使わない場合 *)
fun mapTreeByAuthor f Empty = Empty
  | mapTreeByAuthor f (Node(x, L, R)) = Node(f x, mapTreeByAuthor f L, mapTreeByAuthor f R);

(* 4 *)

fun toPostOrder Empty = ""
  | toPostOrder (Node(s, lt, rt)) = "(" ^ toPostOrder lt ^ ")" ^ "(" ^ toPostOrder rt ^ ")" ^ s;

val testStringTree = Node ("a",Node ("b",Empty,Empty),Node ("c",Node ("d", Empty, Empty),Empty));
toPostOrder testStringTree;

fun toInOrder Empty = ""
  | toInOrder (Node(s, lt, rt)) = "(" ^ toInOrder lt ^ ")" ^ s ^"(" ^ toInOrder rt ^ ")";


toInOrder testStringTree;

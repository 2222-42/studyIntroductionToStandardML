(* SML source file. Copyright (c) by 2222-42 2020.
* Chap 7.2
*)

(* 
case文の評価について
    case exp of pat_1 => exp_1 | ... | pat_n => exp_n

1. expを評価
2. 得られるデータ構造にマッチするpat_iを選択
3. pat_iの中の各変数を、expを評価して得られたデータ構造の中の対応する値に束縛し、この下で式exp_iを評価する
*)

(* 
datatype文で定義されたデータ構造を処理する関数の設計の基本は、
必要な処理を、その型の再帰的な構造に対応した再帰的な処理として記述し、
その再帰的な記述をcase文を使った再帰的関数として実現すること
*)

datatype 'a tree = Empty | Node of 'a * 'a tree * 'a tree;

fun max(x, y) = if x > y then x else y;

(* caseを使った木の高さを求める関数 *)
fun height t =
    case t of Empty => 0
            | Node (_, t1, t2) => 1 + max(height t1, height t2);

(* 関数式と組み合わせた記法を使った木の高さを求める関数 *)
fun heightOther Empty = 0
  | heightOther (Node (_, t1, t2)) = 1 + max(heightOther t1, heightOther t2);

val testNode = Node ("a",Node ("b",Empty,Empty),Node ("c",Node ("d", Empty, Empty),Empty));

fun toPreOrder Empty = ""
  | toPreOrder (Node(s, lt, rt)) = s ^ "(" ^ toPreOrder lt ^ ")" ^ "(" ^ toPreOrder rt ^ ")";

toPreOrder testNode;

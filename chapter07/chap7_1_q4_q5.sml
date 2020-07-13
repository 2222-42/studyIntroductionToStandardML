(* SML source file. Copyright (c) by 2222-42 2020.
* Q7.4 Q 7.5
*)

(* 
Q 7.4
「空の木」を使わず、「子を持たない木」を使った定義
*)

datatype 'a newTree = Leaf of 'a 
                    | Node of 'a * 'a newTree * 'a newTree 
                    | NodeL of 'a * 'a newTree
                    | NodeR of 'a * 'a newTree;

Node ("a", Leaf("b"), NodeL("c", Leaf("d")));


(* SML source file. Copyright (c) by 2222-42 2020.
* Chap 6.2
*)

(* 'a list; *)

(* 
val nil : 'a list
infixr 5 ::
val :: : 'a * 'a list -> 'a list

演算子`::`の型の成約より、要素は全て同じ型を持たなければならに
*) 

1 :: 2 :: 3 :: nil;
(* 
val it = [1,2,3] : int list
- 1 :: 2 :: 3.0 :: nil; 
stdIn:1.2-5.1 Error: operator and operand do not agree [overload conflict]
  operator domain: [int ty] * [int ty] list
  operand:         [int ty] * real list
  in expression:
    2 :: 3.0 :: nil
- 1 :: 2 :: 3;        
stdIn:5.1-5.12 Error: operator and operand do not agree [overload conflict]
  operator domain: [int ty] * [int ty] list
  operand:         [int ty] * [int ty]
  in expression:
    2 :: 3
*)

[];
(* val it = [] : 'a list *)

[1, 2, 3, 4];
(* val it = [1,2,3,4] : int list *)

[fn x => x];
(* val it = [fn] : ('a -> 'a) list *)
(* SML source file. Copyright (c) by 2222-42 2020.
* Chapter 3.4 Q 11
*)

(* 1 *)
fn x => x > 1;
(* 
expect: int -> bool
*)

(* 2 *)
fn x => fn y => fn z => (x y, x "Ada", y > z);
(* 
expect: (string -> 'a) -> string -> string -> (string * 'a * bool)

result:
val it = fn : (string -> 'a) -> string -> string -> 'a * 'a * bool
*)

(* 3 *)
fn x => fn y => y (x > x);
(* 
expect: 'a -> (bool -> 'b)

result:
val it = fn : int -> (bool -> 'a) -> 'a

多重定義されている関数の型のデフォルトはintなので、intが第一引数の型に入る
*)
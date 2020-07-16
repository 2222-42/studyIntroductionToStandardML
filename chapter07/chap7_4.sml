(* SML source file. Copyright (c) by 2222-42 2020.
* Chap 7.4
*)

(* 
リスト型はデータ型
infix 5 ::
datatype 'a list = nil | :: of 'a * 'a list

bool型もデータ型
*)
datatype bool = true | false;

(* だから、パターンマッチが使用できる
bool型に対する特殊構文は、case構文の略記法として定義されている。 

exp1 andalso exp2 
    ==> case exp1 of true => exp2
                   | false => false

exp1 orelse exp2 
    ==> case exp1 of false => exp2
                   | true => true

if exp then exp1 else exp2
    ==> case exp of true => exp1
                  | false => exp2
*)

(* 他にトップレベルで定義されているデータ型の例： *)
(* 比較演算の結果を表現するための型 *)
datatype order = EQUAL | GREATER | LESS;
(* 結果が存在するとは限らないときの型 *)
datatype 'a option = NONE | SOME of 'a;
(* 
exception Option
val valOf : 'a option -> 'a
val getOpt : 'a option * 'a -> 'a
val isSome : 'a option -> bool
*)
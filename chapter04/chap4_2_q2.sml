(* SML source file. Copyright (c) by 2222-42 2020.
* Chap 4.2
*)

(* 
真理値型の演算子は最も結合力の弱い演算子(infix 0, infixr 0)より弱い。
*)

(* 
関数として提供されない理由：
- 計算するうえで必要な式を評価しないようにするため
*)

false andalso true before print "Is this printed?\n";
(* 
expect: 
Is this printed?
val it = false;

result:

val it = false : bool

弱いのだから、
false andalso (true before print "Is this printed?\n");
となっている
*)

true andalso false before print "How about this one?\n";

(* 
expect: 
How about this one?
val it = false;
*)

false orelse true before print "Another one?\n";

(* 
expect: 
Another one?
val it = true;
*)

false andalso true orelse true before print "One more?\n";

(* 
expect: 
val it = false;

result:
One more?
val it = true : bool

before > andalso > orelseであるから、
((false andalso true) orelse (true before print "One more?\n"));
となり、
>> (false orelse (true before print "One more?\n"))
"One more?\n"
>> (false orelse true)
>> true
*)

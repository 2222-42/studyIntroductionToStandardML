(* SML source file. Copyright (c) by 2222-42 2020.
* 
*)

(* 1 *)

(* 
#lと同一の型を持ち、同じ動作をする関数式を、レコードパターンを用いて定義せよ。

#l {..., l=exp, ...} => exp

expected one:

- #Name {Name="name"}; 
val it = "name" : string
- #Age {Name="name"};  
stdIn:2.1-2.19 Error: operator and operand do not agree [record labels]
  operator domain: {Age:'Y; 'Z}
  operand:         {Name:string}
  in expression:
    (fn {Age=Age,...} => Age) {Name="name"}

- #L
stdIn:1.2-10.2 Error: unresolved flex record
   (can't tell what fields there are besides #L)
*)

(* fun take l =
    let
        type record = {l,...}
        (* Error: syntax error: deleting  COMMA DOTDOTDOT RBRACE *)
    in
        (fn {l,...} => l) record
    end;
    
(* 
- fun take l  =
=     fn record => ((fn {l=x,...} => x) record) ;
   (can't tell what fields there are besides #l)
*) *)

(* val take l record = (fn record => (fn {l=x,...}=>x) record); *)

(* val takeL = (fn {L,...} => L); 
takeL {L="string"};
takeL {M="test1", L="test2", N="test3"};
*)



(* これで回答になっているのか？？？？ 
一応`#L {L="string"}`と同じ型、同じエラーが出るから、これでいいのでは？
*)
(fn {L=x,...} => x) {L="string"} ;
(fn {L=x,...} => x) {M="test1", L="test2", N="test3"} ;

(* 
以下のようなのを作りたい

fun takeKeyFromRecord key record =
    (fn {key=x,...} => x) record; 
    
でも以下のようなのを作ろうとした段階で
val takeL = (fn {L=x,...} => x)
stdIn:10.4-42.10 Error: unresolved flex record
   (can't tell what fields there are besides #L) 
ってエラーが表示される。
*)

(* 筆者の解答: SML#を用いているので、なんともいえないが。
• #lと同等の関数式。 SML#のレコード多相を用いれば、実際に関数として定義可能である。 以下はその例である。

# val sharp_l = fn {l,...} => l;
val sharp_l = fn : [’a#{l: ’b}, ’b. ’a -> ’b]
*)


(* 2 *)

(* 
レコードパターンを含む関数式と同一の型を持ち同じ動作をする関数式を、
    fn {l1=x1, l2=x2, ---, ln = xn , ...} => exp
フィールド取り出し演算子を用いて定義せよ
*)

type malt = {x1:string, x2:string, x3:string, x4:int};
val myMalt = {x1 = "Geln Moray", x2 = "Glenlivet", x3 = "the Highlands", x4 = 28};

val recordPattern = fn (record:malt) => (
    let 
        val x1 = #x1 record
        val x2 = #x2 record
    in
        (* exp *)
        x1
    end
);



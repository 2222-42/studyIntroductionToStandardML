(* SML source file. Copyright (c) by 2222-42 2020.
* Chap 8.5 Q 8.10
*)

(* 参照型、値多相性 *)
ref (fn x => x);
(* stdIn:6.1-6.16 Warning: type vars not generalized because of    
   value restriction are instantiated to dummy types (X1,X2,...)
val it = ref fn : (?.X1 -> ?.X1) ref

`?.X1`はコンパイラが導入した不特定の単相型
-> 他のすべての型と異なる
-> 実質的に使用できない
*)

(* 値多相性の制限は参照型と多相型の共存のために導入されたもの *)
(* 与えてしまうと以下のような実行時に型エラーを起こすやばいプログラムがかける *)
(* val polyIdRef = ref (fn x => x);
polyIdRef := (fn x => x + 1);
( !polyIdRef) "You can't add one to me!" *)

(* 値式という概念の導入 *)
(* 多相型を持ちうる式を関数式や定数などに限定している *)
val f = ((fn x => x) 1, fn x => fn y => (x + 1, ref y));
val g = (#2 f) 1;

(* 問8.10 *)
(* 
val f = ((fn x => x) 1, fn x => fn y => (x + 1, ref y));
fはペアを返す関数：
1つ目は1を、
2つ目は、関数を返す
    fn x => fn y => (x + 1, ref y)
これは、a:int を入れたら、
    fn y => (a + 1, ref y)
を、さらにy:Xを入れたら
    (a + 1, ref y: X ref)
を返す。
つまり、型としては
int * (int -> X -> int * X ref)
となるが、しかしながら、変数は多相型を持ちえないので、以下のようになる。
val f = (1,fn) : int * (int -> ?.X1 -> int * ?.X1 ref)
*)

(* 
val g = (#2 f) 1;
gはfの返り値の2つ目に、1を代入する関数。
つまり、
?.X1 -> int * ?.X1 ref
という関数になる。
上述の関数fは`?.X1`というコンパイラが導入した不特定の単相型を導入しているので、
これは、定義することができない。

result:
val g = fn : ?.X1 -> int * ?.X1 ref
-> 定義できた。

以下だったらエラーが発生する。
- val h = (#2 f) 1 1;       
stdIn:1.6-10.12 Error: operator and operand do not agree [overload conflict]
  operator domain: ?.X1
  operand:         [int ty]
  in expression:
    (((fn <pat> => <exp>) f) 1) 1
*)

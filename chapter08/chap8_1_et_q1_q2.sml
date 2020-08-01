(* SML source file. Copyright (c) by 2222-42 2020.
* Chap 8.1　参照型(eqtype t ref)と逐次評価
*)

(* 
MLの変数は、その変数に束縛された値を表す式
    変数の値を更新するといった概念はない

処理するデータ構造によっては、共有変数の更新ができると、効率よいプログラムが書ける場合が多い

この目的のために、特殊なデータ型である `ref` 型構成子が用意されている
*)

(* 
t ref 型:
    t型のデータへのポイント
    MLではこの方をtの参照型と呼ぶ
    tの型に関わらず、同一のポイントか否かの等値性テストが可能なeqtypeである

infix 3 :=
val ref : 'a -> 'a ref
    型構成子でありながら、ref型データのデータ構成子でもある
val ! : 'a ref -> 'a
    値を取り出す
val := : 'a ref * 'a -> unit
    参照型データの値を構成する演算子
*)

val x = ref 1;
!x;
x := 2;
!x;

(* 
式の評価順序
1. 値の定義構文、let ... in exp endでは、順々に評価
2. 組やレコード式などの同一レベルの式が並んでいる場合は、左から順に評価
    レコード式の評価の結果はラベルによってソートされるが、評価順序は表記された式の順序
3. 関数適用(`exp1 exp2`)の評価は、
    a. exp1を評価し、
    b. 関数式`fn x => exp0`を得、
    c. `exp2`を評価し、値vを得
    d. 最後にxをvに束縛し、exp0を評価する

評価順序の制御のための構文
- (exp1; ...; expn)
    exp1からexpnをこの順に評価し、 expnの値をこの式全体の値とする
- exp1 before exp2
    exp1, exp2の順で評価し、exp1をこの式全体の値とする
    ただし、exp2はunit型の式でなければならない
        beforeだから
- while exp1 do exp2
    bool型を持つexp1の評価結果がfalseになるまで、exp1とexp2をこの順に繰り返し評価
    結果は常に`()`である
*)

(* 問 8.1 *)
(* x: int ref *)
(* 以下の式の違いを考えよ *)
!x;
!x before x := !x + 1;
(* xの値を取る評価をし、そののちに、 xの値を取って、それを+1した値をxに入れる
xの値は更新されるが、上記式の段階は表示されない *)
!x;
(x := !x + 1; !x);
(* xの値を取って、それを+1した値をxに入れ、更新されたxの値を評価結果とする *)

(* 問 8.2 *)
(* 他の構文の評価順序の約束を使って定義しよう *)
(* 
(exp1; ...: expn)
は
#n (exp1, ..., expn)
の略記法とみなせる意味で定義となっている
*)

(* exp1 before exp2

問4.1で作った以下で定義することもできる
fun op MyBefore(m, ()) = m;
infix 0 MyBefore;

あまりかっこよくないが、以下のようにletの評価順序を使って、定義することもできる
*)

fun myBefore (exp1, exp2) =
    let
        val returnVal = exp1
        val result2 = (exp2 = ())
    in
        returnVal
    end;
(* もしくは筆者のように *)
fun authorAndMyBefore (e1,e2) = (fn x => fn () => x) e1 e2;


(* while exp1 do exp2 *)
(* fun myWhile (exp1, exp2) = 
    case exp1 of false => ()
                 | true => (exp2; myWhile(exp1, exp2));
これだと、exp1の内容が更新されるまえの状態で引き継がれてしまうから無限ループ？
遅延評価させたい。
いや、(exp1; ...; exp_n)では、exp1からexp_nをこの順に評価していくから、原因は違うのでは？
でも一個だけexp2は実行されている
-> 関数の表現段階で実行されている
*)

fun myWhile() =
    let
        val delayedExp1 = fn () => !x > 0
        val delayedExp2 = fn () => x := !x -1
        fun f() = if delayedExp1() then (delayedExp2(); f()) else ()
    in
        f()
    end;
x := 10;
while (!x > 0) do (x := !x - 1);
x := 10; 
myWhile();

(* 関数みたいに宣言することはできないのだろうか？ *)

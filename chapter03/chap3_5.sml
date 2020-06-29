(* SML source file. Copyright (c) by 2222-42 2020.
* Chap 3.5
*)

(* 
多相型関数
不整合が発生するケース：
- 例外や参照型と一緒に使用するケース

多相型を持ちうる式の限定：
- 実行を伴わないものに限定
  - 実行を伴わない式とは、式そのものが値を表す式のこと。値式と呼ぶ。
  - 実行を伴わない式の例:
    - 定数、変数、関数式、およびこれらを組構成子やデータ構成子(cf: chap. 7)を用いて組み合わせて作られた式
  - 値式の具体例:
    - 1;
    - fib;
    - (1, 2);
    - fn x => fib x
      - 特に関数式`fn p => exp`は、本体`exp`がどのような計算を含んでいても、常に値式
  - 値式ではない(実行を伴う式の)具体例:
    - let val x = 1 in 1 end;
      - (疑問：これは1を出力するときに本体に現れるところに変数xがないがそれに代入するという実行を伴っているからだろうか？)
    - 1 + 1;
    - fib 30;
*)

fun twice f x = f (f x);
(* 
以下は実行時にWarningが起きる上に、望まれる`('a -> 'a) -> 'a -> 'a`のような多相型は得られない。
- val fourTimes = twice twice; 
stdIn:2.5-2.28 Warning: type vars not generalized because of    
   value restriction are instantiated to dummy types (X1,X2,...)
val fourTimes = fn : (?.X1 -> ?.X1) -> ?.X1 -> ?.X1

`X1`はコンパイラーが一時的に作りだした型であり、他の全ての型と異なり、実際には使えない。

- fun f x = x + 1;
val f : int -> int
- fourTiems f;
stdIn:7.1-7.12 Error: operator and operand do not agree [tycon mismatch]
  operator domain: ?.X1 -> ?.X1
  operand:         int -> int
  in expression:
    fourTimes f
*)

(* 以下のように、多相型を与えたい部分を関数式とすれば、意味が同一で望ましい多相型を持った関数が得られる。 *)
val fourTimes = fn f => twice twice f;
